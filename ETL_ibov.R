rm(list = ls())

ini_time <- Sys.time()

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)
library(quantmod)

X <- getSymbols("^BVSP",
                to = "2020-12-31",
                auto.assign = F)
X <- X %>% na.omit
datas <- index(X)

X <- as.data.frame(X)
names(X) <- c("open","high","low","close","volume","close.adj")
X$date <- datas

X <- X[X$volume>0,]

write_fst(X,path = "IBOVESPA",compress = 50)
cat("IBOVESPA");cat(" - ");cat(datas %>% as.character %>% tail(1));cat(" // ")
end_time <- Sys.time()
cat("\n")

print(end_time - ini_time)