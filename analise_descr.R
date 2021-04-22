rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(TTR)
library(DescTools)

# funções auxiliares ----
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

genMatRend <- function(mat_prices_list,spread_DC){
  datas <- rownames(mat_prices_list)
  
  # Matriz de Rendimentos ----
  fcs <- datas %>% 
    as.Date %>% 
    diff %>% 
    mean %>% 
    as.numeric
  
  n <- round(spread_DC/fcs)
  
  rend <- mat_prices_list %>%
    ROC(n=n)
  
  rend
}

# setup ----
type_price <- "close"
spread_DC <- 180

# carregamento dos preços ----
aim_price <- list()

# IBov
precos <- read_fst("IBOVESPA")

aim_price[["IBOVESPA"]] <- precos[,c("date",type_price)]
rownames(aim_price[["IBOVESPA"]]) <- aim_price[["IBOVESPA"]]$date

# Ações
tickers <- list.files("fst/")
for (ticker in tickers) {
  precos <- read_fst(paste0("fst/",ticker))
  aim_price[[ticker]] <- precos[,c("date",type_price)]
  rownames(aim_price[[ticker]]) <- aim_price[[ticker]]$date
}

# matrizes de preço e rendimento ----
price_stocks <- genMatPrice(aim_price)
rend_stocks <- genMatRend(price_stocks,spread_DC) %>% na.omit

N <- nrow(price_stocks)

fcs <- price_stocks %>% 
  rownames %>% 
  as.Date %>% 
  diff %>% 
  mean %>% 
  as.numeric

spread <- round(spread_DC/fcs)

# sumarização ----
set_ativo <- c("Consumo_Varejo","Financeiro","Financeiro","Holding","Consumo_Varejo",
               "Petróleo_Gás","Imobiliário_Construção","Transporte_Logística","Energia_Saneamento",
               "Outros","Siderúrgico_Mineração","Imobiliário_Construção","Energia_Saneamento",
               "Energia_Saneamento","Industrial","Energia_Saneamento","Energia_Saneamento",
               "Siderúrgico_Mineração","Siderúrgico_Mineração","Consumo_Varejo","Holding",
               "Financeiro","Consumo_Varejo","Consumo_Varejo","Consumo_Varejo",
               "Consumo_Varejo","Imobiliário_Construção","Petróleo_Gás","Saúde",
               "Transporte_Logística","Energia_Saneamento","Energia_Saneamento","Siderúrgico_Mineração",
               "Siderúrgico_Mineração","Telecomunicações","Industrial","Outros") 
setores <- set_ativo %>% unique

datas <- rownames(rend_stocks) %>% as.Date
quant_data <- quantile(datas %>% as.numeric,
                       seq(0,1,length.out = 20),
                       names = F) %>% 
  as.Date(origin = "1970-01-01") %>% 
  as.character

sumar <- NULL
for (iter in 2:length(quant_data)) {
  for (s in setores) {
    q <- quant_data[iter]
    dif <- abs(datas - as.Date(q))
    i2 <- which(dif == min(dif))
    
    q0 <- quant_data[iter-1]
    dif <- abs(datas - as.Date(q0))
    i1 <- which(dif == min(dif))
    
    # setorização ----
    dataset <- data.frame(tickers = tickers %>% stringr::str_remove_all("\\.SA"))
    dataset$setor <- set_ativo
    
    dataset$rent.med <- apply(rend_stocks[i1:i2,-1],2,mean,na.rm=T)
    dataset$var <- apply(rend_stocks[i1:i2,-1],2,var,na.rm=T)
    
    dataset$tickers <- dataset$tickers %>% as.factor
    dataset$setor <- dataset$setor %>% as.factor
    
    X <- subset(dataset,setor == s,select = c(rent.med,var))
    Y <- colMeans(X)
    
    param_med <- data.frame(setor = s,
                            data = q,
                            rent.med = Y[1],
                            var.med = Y[2])
    rownames(param_med) <- NULL
    
    sumar <- rbind(sumar,
                   param_med)
  }
}

sumar$setor <- as.factor(sumar$setor)
sumar$data <- as.Date(sumar$data)

rent_ibov <- var_ibov <- NULL
for (iter in 2:length(quant_data)) {
    q <- quant_data[iter]
    dif <- abs(datas - as.Date(q))
    i2 <- which(dif == min(dif))
    
    q0 <- quant_data[iter-1]
    dif <- abs(datas - as.Date(q0))
    i1 <- which(dif == min(dif))
    
    rent_ibov <- c(rent_ibov,
                       mean(rend_stocks[i1:i2,1],na.rm=T))
    var_ibov <- c(var_ibov,
                     var(rend_stocks[i1:i2,1],na.rm=T))
}

ibov <- data.frame(data = quant_data[-1],
                   rent.med = rent_ibov,
                   var = var_ibov)

# análise setorial ----
df_rent <- NULL
for (s in setores) {
  X <- subset(sumar,setor == s,select = c(data,rent.med))
  df_rent <- cbind(df_rent,X$rent.med)
}
colnames(df_rent) <- setores
df_rent <- data.frame(data = X$data,df_rent)

par(mfrow=c(3,4))
for (i in 2:ncol(df_rent)) {
  plot(df_rent$data,df_rent[,i],type = "o",pch = 20,
       main = setores[i-1],
       xlab = "período",ylab = "rentabilidade média")
}

df_var <- NULL
for (s in setores) {
  X <- subset(sumar,setor == s,select = c(data,var.med))
  df_var <- cbind(df_var,X$var.med)
}
colnames(df_var) <- setores
df_var <- data.frame(data = X$data,df_var)

par(mfrow=c(3,4))
for (i in 2:ncol(df_var)) {
  plot(df_var$data,df_var[,i],type = "o",pch = 20,
       main = setores[i-1],
       xlab = "período",ylab = "volatilidade média")
}

# análise de periódica ----
rend_per <- NULL
for (q in quant_data[-1]) {
  X <- subset(sumar,data == q,select = c(data,rent.med))
  rend_per <- cbind(rend_per,
                    X$rent.med %>% mean)
}

par(mfrow=c(1,1))
plot(quant_data[-1] %>% as.Date,rend_per,
     main="Rentabilidade média por período",
     xlab = "data",ylab = "rentabilidade média",
     type = "o",pch=20)

var_per <- NULL
for (q in quant_data[-1]) {
  X <- subset(sumar,data == q,select = c(data,var.med))
  var_per <- cbind(var_per,
                    X$var.med %>% mean)
}

plot(quant_data[-1] %>% as.Date,var_per,
     main="Volatilidade média por período",
     xlab = "data",ylab = "volatilidade média",
     type = "o",pch=20)

# análise de correlação ----
mat_cor <- cor(cbind(IBov = ibov$rent.med,
                     df_rent[,-1]))
corrplot::corrplot(mat_cor,method = "color",
                   tl.cex = 0.75,tl.srt = 45)
