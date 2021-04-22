rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(TTR)

library(parallel)
library(doParallel)

options(warn = -1)

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
  
  mi_imp <- quantile(feat_imp,0.4,names = F) # mean(feat_imp) 
  pred_sel <- rownames(feat_imp)[feat_imp[,1]>=mi_imp]
  
  pred_sel
}

# setup ----
type_price <- "close"
lag_DC <- 180

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
rend_stocks <- genMatRend(price_stocks,lag_DC)

N <- nrow(price_stocks)

fcs <- price_stocks %>% 
  rownames %>% 
  as.Date %>% 
  diff %>% 
  mean %>% 
  as.numeric

lag <- round(lag_DC/fcs)

# datasets de treino ----
df_full <- NULL
for (ticker in tickers) {
  P <- price_stocks[,ticker]
  
  # Ind Técnicos
  macd <- MACD(P,
               nFast = round(lag/2),nSlow = round(2/3*lag),nSig = round(lag/3))
  rsi <- RSI(P,n = lag/2)
  mmr <- SMA(P,round(lag/2))
  mml <- SMA(P,lag)
  cti <- CTI(P,n = lag/2)
  
  colnames(mmr) <- "mmr"
  colnames(mml) <- "mml"
  
  # Mercado
  roc_ibov <- rend_stocks[,"IBOVESPA"]
  volat_ibov <- roc_ibov %>% movMeas(f=sd,n=2*lag)
  
  roc_ibov <- matrix(roc_ibov,ncol=1)
  volat_ibov <- matrix(volat_ibov,ncol=1)
  
  colnames(roc_ibov) <- "roc_ibov"
  colnames(volat_ibov) <- "volat_ibov"
  
  # Ação
  roc_acao <- rend_stocks[,ticker]
  volat_acao <- roc_acao %>% movMeas(f=sd,n=2*lag)
  
  roc_acao <- matrix(roc_acao,ncol=1)
  volat_acao <- matrix(volat_acao,ncol=1)
  
  colnames(roc_acao) <- "roc_acao"
  colnames(volat_acao) <- "volat_acao"
  
  # dummies de preço
  dP <- c(NA,P %>% EMA(10) %>% diff) %>% matrix(ncol = 1)
  d2P <- c(NA,NA,P %>% EMA(10) %>% diff(differences = 2)) %>% matrix(ncol = 1)
  
  zP <- P %>% movMeas(f=last_norm,n=lag) %>% matrix(ncol=1)
  
  colnames(dP) <- "dP"
  colnames(d2P) <- "d2P"
  colnames(zP) <- "zP"
  
  # dummies de rendimento
  drcc <- c(NA,roc_acao[,1] %>% diff) %>% matrix(ncol = 1)
  d2rcc <- c(NA,NA,roc_acao[,1] %>% diff(differences = 2)) %>% matrix(ncol = 1)
  
  rcc2 <- roc_acao^2
  
  mmr_rcc <- roc_acao %>% SMA(n=round(lag/2)) %>% matrix(ncol=1)
  mml_rcc <- roc_acao %>% SMA(n=lag) %>% matrix(ncol=1)
  
  zrcc <- roc_acao[,1] %>% movMeas(f=last_norm,n=lag) %>% matrix(ncol=1)
  
  colnames(drcc) <- "drcc"
  colnames(d2rcc) <- "d2rcc"
  colnames(rcc2) <- "rcc2"
  colnames(mmr_rcc) <- "mmr_rcc"
  colnames(mml_rcc) <- "mml_rcc"
  colnames(zrcc) <- "zrcc"
  
  # montagem
  df0 <- cbind(macd,rsi,mmr,mml,cti,
               roc_ibov,volat_ibov,
               roc_acao,volat_acao,
               dP,d2P,zP,
               drcc,d2rcc,rcc2,mmr_rcc,mml_rcc,zrcc)
  
  # defasagem
  df <- data.frame(outcome = roc_acao[(1+lag):N],
                   df0[1:(N-lag),]) %>% na.omit
  
  datas <- df %>% 
    rownames %>% 
    as.Date
  
  # eliminação macd
  df$macd <- NULL
  df$ticker <- ticker
  
  df_full <- rbind(df_full,df)
}

# write.csv(df_full,"datasets_treino.csv")
write_fst(df_full,"datasets_treino.fst")
