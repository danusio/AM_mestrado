rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(TTR)
library(doParallel)
library(parallel)
library(tseries)
library(forecast)

# options(warn = -1)

# funções auxiliares ----
movMeas <- function(x,f,n,...){
  N <- length(x)
  
  out <- rep(NA,n-1)
  for (i in n:N) {
    v <- matrix(x[(i-n+1):i],nrow = 1)
    out <- c(out,
             apply(v,1,f,...))
  }
  out
}

attrNA <- function(x){
  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}

genMatPrice <- function(prices_list){
  num_tick <- length(prices_list)
  tickers <- names(prices_list)
  
  # Sincronização ----
  datas <- prices_list[[1]][,1] %>% 
    as.character
  for (i in 2:num_tick) {
    datas <- c(datas,
               prices_list[[i]][,1] %>% 
                 as.character) %>% 
      unique
  }
  datas <- datas %>% 
    as.Date %>% 
    sort %>% 
    as.character
  
  # Merging ----
  mat_prices_list <- NULL
  for (i in 1:num_tick) {
    mat_prices_list <- cbind(mat_prices_list,
                             ifelse(datas %in% (prices_list[[i]][,1] %>% 
                                                  as.character),
                                    prices_list[[i]][datas,2],
                                    NA))
  }
  
  rownames(mat_prices_list) <- datas
  colnames(mat_prices_list) <- tickers
  
  # Atribuição de valores faltantes ----
  mat_prices_list <- mat_prices_list %>% apply(2,attrNA)
  mat_prices_list <- mat_prices_list %>% na.omit
  
  datas <- rownames(mat_prices_list)
  
  mat_prices_list
}

genMatRend <- function(mat_prices_list,lag_DC){
  datas <- rownames(mat_prices_list)
  
  # Matriz de Rendimentos ----
  fcs <- datas %>% 
    as.Date %>% 
    diff %>% 
    mean %>% 
    as.numeric
  
  n <- round(lag_DC/fcs)
  
  rend <- mat_prices_list %>%
    ROC(n=n)
  
  rend
}

# setup ----
type_price <- "close"
lag_DC <- 180
samp_size <- 100

# carregamento dos preços ----
aim_price <- list()

# IBov
precos <- read_fst("IBOVESPA")

aim_price[["IBOVESPA"]] <- precos[,c("date",type_price)]
rownames(aim_price[["IBOVESPA"]]) <- aim_price[["IBOVESPA"]]$date

# Ações
tickers <- list.files("fst/")
ntick <- length(tickers)

for (ticker in tickers) {
  precos <- read_fst(paste0("fst/",ticker))
  aim_price[[ticker]] <- precos[,c("date",type_price)]
  rownames(aim_price[[ticker]]) <- aim_price[[ticker]]$date
}

# matrizes de preço e rendimento ----
price_stocks <- genMatPrice(aim_price)
fcs <- price_stocks %>% 
  rownames %>% 
  as.Date %>% 
  diff %>% 
  mean %>% 
  as.numeric

lag <- round(lag_DC/fcs)

rend_stocks <- genMatRend(price_stocks,lag_DC)

tick_sel <- c(6,7,10,15,16,18,27,35,36,37)
tickers_sel <- tickers[tick_sel]

mat_rcc <- rend_stocks[,tick_sel] %>% na.omit
rm(price_stocks,rend_stocks)
N <- nrow(mat_rcc)

# Dickey-Fuller ----
prob_val <- apply(mat_rcc,2,
                  function(x) adf.test(x,alternative = "stationary",lag)$p.value)

cat("p-value < 0.05 --> Série estacionária: \n\n")
print(prob_val)
cat("\n")

is_stat <- ifelse(prob_val<0.05,T,F)

# paralelização ----
Mycluster = makeCluster(detectCores()-2,
                        type = "FORK")
registerDoParallel(Mycluster)

# Auto-ARIMA ----
datas <- rownames(mat_rcc[-((N-lag+1):N),]) %>% as.Date
data_final <- tail(datas,1)
dif <- (datas - (data_final - 5*365)) %>% as.numeric %>% abs
ini <- which(dif == min(dif))
data_inicial <- datas[ini]

int_teste <- round(seq(ini,length(datas),
                       length.out = samp_size))

t0 <- Sys.time()

# r2 <- mae <- 
predicoes <- observ <- NULL
for (i in 1:length(tickers_sel)) {
  outpred <- NULL
  for (j in int_teste) {
    arima_model <- auto.arima(mat_rcc[1:(j-1),i],
                              stationary = is_stat[i],
                              nmodels = 20)
    outpred <- c(outpred,
                 forecast(arima_model,h=lag)$mean %>% 
                   tail(1) %>% as.numeric)
  }
  
  predicoes <- cbind(predicoes,outpred)
  observ <- cbind(observ,mat_rcc[int_teste+lag,i])
  
  # r2 <- c(r2,R2(outpred,mat_rcc[int_teste+lag,i]))
  
  # residuo <- outpred-mat_rcc[int_teste+lag,i]
  # mae <- c(mae,residuo %>% abs %>% mean)
  
  # plot(residuo %>% density,main = tickers_sel[i])
}

t1 <- Sys.time()

# setup inicial de paralelização ----
stopCluster(Mycluster)
registerDoSEQ()

colnames(predicoes) <- paste0(tickers_sel,".pred")
colnames(observ) <- paste0(tickers_sel,".obs")

# names(r2) <- names(mae) <- tickers_sel

# exibição ----
deltaT <- as.numeric(t1) - as.numeric(t0)

if (deltaT < 60) {
  cat("Tempo de processamento (s): ")
  cat(deltaT)
}else{
  if (deltaT < 3600) {
    cat("Tempo de processamento (min): ")
    cat(deltaT/60)
  }else{
    cat("Tempo de processamento (h): ")
    cat(deltaT/3600)
  }
}
cat("\n\n")

write.csv(predicoes,"predicoes_arima.csv")
write.csv(observ,"observ_arima.csv")
