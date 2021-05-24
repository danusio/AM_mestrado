rm(list = ls())

setwd("~/Documentos/MESTRADO - PO/APRENDIZADO DE MÁQUINA")

library(magrittr)
library(ggplot2)
library(stringr)
library(fmsb)

# Tempo de Execução ----
tempo <- read.csv("tempo_exec.csv",row.names = 1)
hist(tempo$time,main = "Tempo por Iteração",
     xlab = "tempo (s)")

tempo$time %>% summary %>% print
cat("\n")

# Carregamento dos Dados ----
feats <- read.csv("features.csv",
                  row.names = 1,stringsAsFactors = T)

pred <- read.csv("predicoes.csv",row.names = 1)
obs <- read.csv("observ.csv",row.names = 1)

# Gráfico - Importância das Features ----
# ocorrência das 5 features mais frequentes em cada uma das 9 posições de preditores selecionados.
# A 'Importância 1' descreve a variável mais importante selecionada.

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
                  ncol = 3,nrow = 3) %>% 
  plot

# Quantificação de Importância ----
# Etapas do cálculo de importância de uma feature:
# 1 - Determinar as features que foram selecionadas.
# 2 - Calcular a frequência percentual de ocorrência da feature em cada posição (1 a 9)
# 3 - Multiplicar o resultado anterior por 1, para a última posição, por 2, para a penúltima, e assim por diante.
# 4 - Somar os resultados para cada feature.

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

# Sumarização de Regressão ----
statSummary <- function(pred,obs){
  tickers <- strtrim(colnames(pred),5)
  ntick <- ncol(pred)
  
  # Métricas da regressão ----
  # 1. R2 e MAE
  r2 <- mae <- NULL
  for (i in 1:ntick) {
    r2 <- c(r2,caret::R2(pred[,i],obs[,i]))
    mae <- c(mae,caret::MAE(pred[,i],obs[,i]))
  }
  names(r2) <- names(mae) <- tickers
  
  # print(sumar)
  
  # 2. Índice de Willmott
  willmott <- function(pred,obs) 1-sum((pred-obs)^2)/sum((abs(pred-mean(obs))+abs(obs-mean(obs))^2))
  
  WI <- NULL
  for (i in 1:ntick) {
    WI <- c(WI,willmott(pred[,i],obs[,i]))
  }
  
  names(WI) <- tickers
  
  # cat("\nÍndices de Willmott: \n\n")
  # print(WI)
  
  # 3. curtose dos desvios de predição
  # curtose de uma curva normal é em torno de 3
  
  curtose <- NULL
  for (i in 1:ntick) {
    curtose <- c(curtose,
                 moments::kurtosis(pred[,i]-obs[,i]))
  }
  
  names(curtose) <- tickers
  
  # cat("\nCurtose: \n\n")
  # print(curtose)
  
  # Testes estatísticos ----
  # Todos os p-values, dos testes de Wilcoxon e F, confirmaram a hipótese nula:
  # mediana das predições = mediana dos dados reais
  # variância das predições = variância dos dados reais
  # alfa = 0.05
  
  pval_w <- pval_F <- NULL
  for (i in 1:ntick) {
    testeW <- wilcox.test(pred[,i],obs[,i])
    testeF <- var.test(pred[,i],obs[,i])
    
    pval_w <- c(pval_w,testeW$p.value)
    pval_F <- c(pval_F,testeF$p.value)
  }
  
  names(pval_F) <- names(pval_w) <- tickers
  
  # cat("\np-values do teste de Wilcoxon: \n\n")
  # print(pval_w)
  # cat("\np-values do teste F: \n\n")
  # print(pval_F)
  
  out <- cbind(r2,mae,WI,curtose,pval_w,pval_F)
  colnames(out) <- c("R2","MAE","WI","kurtosis","Wilcoxon","F")
  
  out
}

summary_ticker <- statSummary(pred,obs)

cat("Modelo porposto:\n")
summary_ticker %>% round(6) %>% print
cat("\n")

perf <- statSummary(pred,obs) %>% colMeans

perf %>% round(6) %>% print
cat("\n")

# Desempenho ARIMA ----
pred_arima <- read.csv("predicoes_arima.csv",row.names = 1)
obs_arima <- read.csv("observ_arima.csv",row.names = 1)

cat("ARIMA:\n")
statSummary(pred_arima,obs_arima) %>% round(6) %>% print
cat("\n")

perf_arima <- statSummary(pred_arima,obs_arima) %>% colMeans
perf_arima %>% round(6) %>% print
cat("\n")

# Desempenho Regressão Linear ----
# pred_lm <- read.csv("predicoes_lm.csv",row.names = 1)
# obs_lm <- read.csv("observ_lm.csv",row.names = 1)
# 
# cat("Regressão Linear:\n")
# statSummary(pred_lm,obs_lm) %>% round(6) %>% print
# cat("\n")
# 
# perf_lm <- statSummary(pred_lm,obs_lm) %>% colMeans
# perf_lm %>% round(6) %>% print
# cat("\n")

# gráfico radar ----
# campos: R2, MAE, WI, curtose

dados <- rbind(perf[1:4],perf_arima[1:4]) %>% 
  data.frame

# Range de variação das medidas
limits <- data.frame(R2 = c(0,1),
                     MAE = c(0,0.5),
                     WI = c(0,1),
                     kurtosis = c(0,15))
# gráfico
radarchart(rbind(limits,dados),
           axistype = 1,
           pcol = c(2,1),plty = c(1,1,1),
           title = "Comparação com ARIMA",
           pfcol=c(rgb(0.8,0.2,0.5,0.4),
                   rgb(0.2,0.5,0.5,0.4)),
           axislabcol = "grey25",
           caxislabels = c("0","25","50","75"))

# limits <- data.frame(R2 = c(0.85,1),
#                      MAE = c(0,0.05),
#                      WI = c(0.98,1),
#                      kurtosis = c(0,30))
# radarchart(rbind(limits,
#                  data.frame(summary_ticker[,1:4])),
#            plty = c(1,1,1),
#            title = "Comparação entre Papéis")
