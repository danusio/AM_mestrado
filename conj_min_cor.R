rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(genalg)

library(parallel)
library(doParallel)

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

last_norm <- function(x) (tail(x,1) - mean(x))/sd(x)

featSel <- function(df){
  imp1 <- oneR(outcome ~ .,df)
  imp2 <- information.gain(outcome ~ .,df)
  imp3 <- chi.squared(outcome ~ .,df)
  
  mod1 <- preProcess(imp1,method = "range",rangeBounds = c(0,1))
  mod2 <- preProcess(imp2,method = "range",rangeBounds = c(0,1))
  mod3 <- preProcess(imp3,method = "range",rangeBounds = c(0,1))
  
  imp_norm1 <- predict(mod1,imp1)
  imp_norm2 <- predict(mod2,imp2)
  imp_norm3 <- predict(mod3,imp3)
  
  feat_imp <- ((imp_norm1+imp_norm2+imp_norm3)/3) %>%
    apply(2,sort,decreasing=T)
  
  mi_imp <- quantile(feat_imp,0.5,names = F) # mean(feat_imp) 
  pred_sel <- rownames(feat_imp)[feat_imp[,1]>=mi_imp]
  
  pred_sel
}

# setup ----
type_price <- "close"
lag_DC <- 180
samp_size <- 20

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
rend_stocks <- genMatRend(price_stocks,lag_DC)

N <- nrow(price_stocks)

fcs <- price_stocks %>% 
  rownames %>% 
  as.Date %>% 
  diff %>% 
  mean %>% 
  as.numeric

lag <- round(lag_DC/fcs)

# correlação ----
mat_cor <- cor(rend_stocks,use = "c") %>% abs
mat_cor[lower.tri(mat_cor,diag = T)] <- NA

# set.seed(1)
# x0 <- sample(c(0,1),length(tickers),replace = T,prob = c(0.6,0.4))

n <- 10

fit <- function(x){
  if (sum(x)<1) {
    return(1e6)
  }
  
  if (sum(x)<n) {
    return(sum(x)*1e6)
  }
  
  
  ind <- (1:length(x))[x==1]
  
  y <- mat_cor[ind,ind] %>% 
    matrix(ncol = 1) %>% 
    na.omit %>% 
    as.numeric
  
  return(mean(y))
}

# AG ----
ans <- rbga.bin(size = ntick,
                popSize = 500,
                iters = 100,
                evalFunc = fit)

plot(ans)

# melhores conjuntos
fit_value <- apply(ans$population,1,fit)

min_ind <- which(fit_value == min(fit_value))

df_elite <- ans$population[min_ind,] %>% unique

for (i in 1:nrow(df_elite)) {
  ind <- (1:ntick)[df_elite[i,]==1]
  
  y <- mat_cor[ind,ind] %>% 
    matrix(ncol = 1) %>% 
    na.omit %>% 
    as.numeric
  
  cat("Conjunto de ações: \n\n")
  print(tickers[ind])
  cat("\nEstatísticas das correlações: \n")
  print(summary(y))
}
