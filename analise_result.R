rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(ggplot2)

feats <- read.csv("Resultados_features_1a10.csv",
                  row.names = 1,stringsAsFactors = T)
meas <- read.csv("Resultados_measures1a10.csv")

# Gráfico - Importância das Features ----
plots <- list()
for (i in 2:10) {
  df <- data.frame(table(feats[,i]))
  df$Freq <- df$Freq/sum(df$Freq)*100
  
  df <- df[1:5,]
  
  plots[[i-1]] <- ggplot(df,aes(reorder(Var1,-Freq,sum),Freq)) +
    geom_col(fill="black") +
    labs(x="features",y="% de ocorrência",
         title = paste0("Importância ",i-1)) + 
    theme(plot.title = element_text(size=12))
  
  # element_text(family = NULL, face = NULL, colour = NULL, size = NULL,
  #              hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
  #              color = NULL)
}

ggpubr::ggarrange(plotlist = plots,
                  ncol = 3,nrow = 3)

# Quantificação de Importância ----
list_imp <- apply(feats[,2:10],2,table)

list_imp <- lapply(list_imp, 
                   function(x) x/sum(x))

varnames <- sapply(list_imp, names) %>% 
  unlist(use.names = F) %>% 
  unique

n <- length(varnames)

imp <- rep(0,n)
for (i in 1:length(list_imp)) {
  x <- list_imp[[i]][varnames]*(n:1)
  x[is.na(x)] <- 0
  names(x) <- varnames
  imp <- imp+x
}

imp <- sort(imp,decreasing = T)

plot(imp,pch=16,type = "o",
     main = "Importância dos Preditores",
     ylab = "importância")

# Métricas da regressão ----

sumar <- apply(meas[,-1], 2, 
               function(x) c(mean(x),sd(x)))
rownames(sumar) <- c("média","desvpad")

print(sumar)
