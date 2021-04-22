rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(fst)

X <- read_fst("datasets_treino.fst")
write.csv(X,"datasets_treino.csv")