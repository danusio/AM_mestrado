rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(TTR)
library(caret)
library(FSelector)

# FUNÇÕES ----
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
  # atribui o elemento imediatamente anterior a um valor faltante
  # x: vetor
  # value: vetor com os NA's substituidos, exceto os primeiros 'missing values'.
  
  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}

genMatPrice <- function(prices_list){
  # converte uma lista de preços em uma matriz de preços sincronizados
  # prices_list: lista com data frames date-price
  # value: matriz de preços sincronizados
  
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
  # converte uma matriz de preços em uma matriz de rendimentos sincronizados
  # prices_list: lista com data frames date-price
  # spread_DC: spread dos rendimentos, em dias corridos
  # value: matriz de rendimentos sincronizados
  
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

normalize <- function(x) (x - mean(x))/sd(x)

last_norm <- function(x) (tail(x,1) - mean(x))/sd(x)

featSel <- function(df){
  feat_imp <- information.gain(outcome ~ .,df) %>%
    apply(2,sort,decreasing=T)
  
  mi_imp <- quantile(feat_imp,0.4,names = F)
  pred_sel <- rownames(feat_imp)[feat_imp[,1]>=mi_imp]
  
  # pred_sel <- rownames(feat_imp)[1:n]
  
  pred_sel
}

# SETUP ----
type_price <- "close"
spread_DC <- 180

# CARREGAMENTO DOS DADOS ----
aim_price <- list()

precos <- read_fst("IBOVESPA")

aim_price[["IBOVESPA"]] <- precos[,c("date",type_price)]
rownames(aim_price[["IBOVESPA"]]) <- aim_price[["IBOVESPA"]]$date

tickers <- list.files("fst/")
for (ticker in tickers) {
  precos <- read_fst(paste0("fst/",ticker))
  aim_price[[ticker]] <- precos[,c("date",type_price)]
  rownames(aim_price[[ticker]]) <- aim_price[[ticker]]$date
}

# MATRIZES DE PREÇOS E RENDIMENTOS
price_stocks <- genMatPrice(aim_price)
rend_stocks <- genMatRend(price_stocks,spread_DC)

N <- nrow(price_stocks)

fcs <- price_stocks %>% 
  rownames %>% 
  as.Date %>% 
  diff %>% 
  mean %>% 
  as.numeric

spread <- round(spread_DC/fcs)

# FEATURES DE INDICADORES TÉCNICOS ----
ticker <- "PETR4.SA"
P <- price_stocks[,ticker]

macd <- MACD(P)
rsi <- RSI(P)
mmr <- SMA(P,round(spread/2))
mml <- SMA(P,spread)
cti <- CTI(P)

colnames(mmr) <- "mmr"
colnames(mml) <- "mml"

# FEATURES DE MERCADO ----
roc_ibov <- rend_stocks[,"IBOVESPA"]
volat_ibov <- roc_ibov %>% movMeas(f=sd,n=2*spread)

roc_ibov <- matrix(roc_ibov,ncol=1)
volat_ibov <- matrix(volat_ibov,ncol=1)

colnames(roc_ibov) <- "roc_ibov"
colnames(volat_ibov) <- "volat_ibov"

# FEATURES DA AÇÃO ----
roc_acao <- rend_stocks[,ticker]
volat_acao <- roc_acao %>% movMeas(f=sd,n=2*spread)

roc_acao <- matrix(roc_acao,ncol=1)
volat_acao <- matrix(volat_acao,ncol=1)

colnames(roc_acao) <- "roc_acao"
colnames(volat_acao) <- "volat_acao"

# FEATURES ARTIFICIAIS DE PREÇO ----
dP <- c(NA,P %>% diff) %>% matrix(ncol = 1)
d2P <- c(NA,NA,P %>% diff(differences = 2)) %>% matrix(ncol = 1)

zP <- P %>% movMeas(f=last_norm,n=spread) %>% matrix(ncol=1)

colnames(dP) <- "dP"
colnames(d2P) <- "d2P"
colnames(zP) <- "zP"

# FEATURES ARTIFICIAIS DE ROC ----
drcc <- c(NA,roc_acao[,1] %>% diff) %>% matrix(ncol = 1)
d2rcc <- c(NA,NA,roc_acao[,1] %>% diff(differences = 2)) %>% matrix(ncol = 1)

rcc2 <- roc_acao^2

mmr_rcc <- roc_acao %>% SMA(n=round(spread/2)) %>% matrix(ncol=1)
mml_rcc <- roc_acao %>% SMA(n=spread) %>% matrix(ncol=1)

zrcc <- roc_acao[,1] %>% movMeas(f=last_norm,n=spread) %>% matrix(ncol=1)

colnames(drcc) <- "drcc"
colnames(d2rcc) <- "d2rcc"
colnames(rcc2) <- "rcc2"
colnames(mmr_rcc) <- "mmr_rcc"
colnames(mml_rcc) <- "mml_rcc"
colnames(zrcc) <- "zrcc"

# MONTAGEM DO DATASET ----
df0 <- cbind(macd,rsi,mmr,mml,cti,
             roc_ibov,volat_ibov,
             roc_acao,volat_acao,
             dP,d2P,zP,
             drcc,d2rcc,rcc2,mmr_rcc,mml_rcc,zrcc)

df <- data.frame(outcome = roc_acao[(1+spread):N],
                 df0[1:(N-spread),]) %>% na.omit

df$macd <- NULL # <- df$mmr <- df$mmr_rcc

# MODELAGEM ----
n <- nrow(df)
ini_teste <- round(0.5*n)

t0 <- Sys.time()

outpred <- NULL
for (i in ini_teste:n) {
  cat("Iteração ");cat(i-ini_teste+1);cat(" // ")
  modelo_norm <- df[1:(i-1),-1] %>% preProcess
  
  df_treino <- cbind(outcome=df$outcome[1:(i-1)],
                     predict(modelo_norm,df[1:(i-1),-1]))
  
  # FEATURE SELECTION ----
  pred_sel <- featSel(df_treino,n=6)
  df1 <- df_treino[,c("outcome",pred_sel)]
  
  modelo_knn <- train(outcome ~ .,
                      df1,
                      method = "knn",
                      tuneGrid = expand.grid(k=5),
                      metric = "MAE",
                      trControl = trainControl(method = "repeatedcv",
                                               repeats = 3,
                                               number = 10,
                                               summaryFunction = defaultSummary))
  outpred <- c(outpred,
               predict(modelo_knn,
                       predict(modelo_norm,df[i,-1])))
}

t1 <- Sys.time()

deltaT <- as.numeric(t1) - as.numeric(t0)

r2 <- R2(outpred,df$outcome[ini_teste:n])

residuo <- outpred-df$outcome[ini_teste:n]
mae <- residuo %>% abs %>% mean

plot(residuo %>% density)

cat("\n\n")
cat("R²: ");cat(r2);cat("\n")
cat("MAE: ");cat(mae);cat("\n")

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
