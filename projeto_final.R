rm(list = ls())

library(magrittr)
library(quantmod)
library(forecast)

options(warn = -1)

precos <- getSymbols("bova11.sa",auto.assign = F)

fec <- Cl(precos)

summaryARIMA <- function(x,n=1){
  rcc <- ROC(x,n) %>% na.omit
  
  # Dickey-Fuller
  testeDF <- adf.test(rcc,alternative = "stationary")
  cat("Estacionariedade:\n")
  print(testeDF)
  cat("------------------------\n")
  # Auto ARIMA
  model <- auto.arima(rcc)
  cat("\nModelo:\n")
  print(model)
  cat("------------------------\n")
  cat("\nCoeficientes ARIMA: \n")
  print(model$coef)
}

for (n in c(1,5,10,20,40,80,160)) {
  cat("\n\nHorizonte de rendimentos: ");cat(n)
  cat("\n---------------------\n")
  summaryARIMA(fec,n)
}