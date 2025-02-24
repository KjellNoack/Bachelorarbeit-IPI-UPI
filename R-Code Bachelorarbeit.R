
library(readxl) # einlesen von csv Dateien
library(vars) # fÃ¼r VAR Analysen
library(ggplot2) # zum plotten
library(dplyr)
library(Metrics) # fÃ¼r mse etc.
library(lmtest) # FÃ¼r Granger KausalitÃ¤t und statistische Tests
library(tseries) # zum testen von StationaritÃ¤t durch adf
library(NlinTS) # zum testen von StationaritÃ¤t durch ad
library(olsrr) # FÃ¼r Variablen Selection (Forword etc.)


IP_original <- read.delim("~/TU Dortmund/Bachelorarbeit/Industrieproduktion.txt", header=FALSE) # Ab 12.2000 bis 08.2024
IP_original <- IP_original$V1
IP_original <- as.numeric(gsub(",", ".", IP_original))
IP_difflog <- diff(log(IP_original))
IP_train <- ts(IP_difflog[1:228])

Zins_original <- read.delim("~/TU Dortmund/Bachelorarbeit/Geldmarktzinssatz.txt", header=FALSE, na.strings="") # Ab 01.2001 bis 08.2024
Zins <- Zins_original$V1
Zins_train <- ts(Zins[1:228])


Arbeit_original <- read.table("~/TU Dortmund/Bachelorarbeit/Arbeitslosenquote.txt", quote="\"", comment.char="") # Ab 01.2001 bis 08.2024
Arbeit <- Arbeit_original$V1
Arbeit_train <- ts(Arbeit[1:228])

VPI_original <- read.table("~/TU Dortmund/Bachelorarbeit/VPI.txt", quote="\"", comment.char="") # Ab 01.2005 bis 08.2021
VPI <- VPI_original$V1
VPI_train <- ts(VPI[1:228])


IPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_data_08_2024.csv", sep = ",")  
IPI <- IPI_original$ipi
IPI <- as.numeric(IPI)
IPI_train <- ts(IPI[1:228])

subIPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_subdata_08_2024.csv", sep = ",")
subIPI <- data.frame(apply(subIPI_original[, 2:11], 2, as.numeric))
subIPI_train <- subIPI[1:228, ]

UPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_data_10_2024.csv", sep = ",") 
UPI <- UPI_original$upi[-c(285,286)]
UPI <- as.numeric(UPI)
UPI_train <- ts(UPI[1:228]) 

subUPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_subdata_10_2024.csv", sep = ",") 
subUPI <- data.frame(apply(subUPI_original[-c(285, 286), 2:15], 2, as.numeric))
subUPI_train <- subUPI[1:228, ]

EPU_original <- read_excel("TU Dortmund/Bachelorarbeit/Europe_Policy_Uncertainty_Data.xlsx")
EPU <- EPU_original$Germany_News_Index[-c(1:168, 453, 454)] # Ab 01.2001 bis 08.2024
EPU_train <- ts(EPU[1:228])



#### Visualisierung der Zeitreihen

'df_features <- data.frame(
  time = rep(c(rep(1:12, 19), 1:8),4),
  values = c(Arbeit, VPI, IP, Zins),
  features = factor(rep(1:4, each = 236),
                    labels = c("Arbeit", "VPI", "IP", "Zins"))
)'


temp_Arbeit <- data.frame(Arbeit = Arbeit, x = 1:284)
ggplot(temp_Arbeit, aes(x = x, y = Arbeit)) +
  geom_line()

temp_VPI <- data.frame(VPI = VPI, x = 1:284)
ggplot(temp_VPI, aes(x = x, y = VPI)) +
  geom_line()

temp_IP_difflog <- data.frame(IP_difflog = IP_difflog, x = 1:284)
ggplot(temp_IP_difflog, aes(x = x, y = IP_difflog)) + 
  geom_line()

temp_Zins <- data.frame(Zins = Zins, x = 1:284)
ggplot(temp_Zins, aes(x = x, y = Zins)) +
  geom_line()



plot_IPI <- data.frame(IPI = IPI, x = 1:284)
ggplot(plot_IPI, aes(x = x, y = IPI)) +
  geom_line()

plot_UPI <- data.frame(UPI = UPI, x = 1:284)
ggplot(plot_UPI, aes(x = x, y = UPI)) +
  geom_line()

plot_EPU <- data.frame(EPU = EPU, x = 1:284)
ggplot(plot_EPU, aes(x = x, y = EPU)) +
  geom_line()





# Test auf StationaritÃ¤t

# Dickey-Fuller Test 
df_test_Zins <- df.test(Zins_train, 1)
df_test_Zins$summary()
df_test_Arbeit <- df.test(Arbeit_train, 1)
df_test_Arbeit$summary()
df_test_IP <- df.test(IP_train, 1)
df_test_IP$summary()
df_test_VPI <- df.test(VPI_train, 1)
df_test_VPI$summary()
df_test_IPI <- df.test(IPI_train, 1)
df_test_IPI$summary()
df_test_UPI <- df.test(UPI_train, 1)
df_test_UPI$summary()
df_test_EPU <- df.test(EPU_train, 1)
df_test_EPU$summary()


#### Ablauf einer Prediction ####

# AIC und BIC berechnen fÃ¼r unterschiedliche lags
'min_BIC <- function(...){
  var <- ts.union(...)
  aic <- numeric(24)
  bic <- numeric(24)
  for(i in 1:24){
    aic[i] <- AIC(VAR(var, p = i))
    bic[i] <- BIC(VAR(var, p = i))
  }
  print(data.frame(AIC = aic, BIC = bic))
}'

# Features in die Reihenfolge nach Lags umwandeln
combine_vectors <- function(...){
  features <- list(...)
  result <- rev(as.vector(t(data.frame(features))))
  return(result)
}

#temp <- combine_vectors(IP_difflog[168:182], Zins[168:182],
#                         Arbeit[168:182], VPI[168:182])



# Erstellung von rolling Window Funktion





prediction <- function(features, coeff){
  result <- numeric(ncol(coeff))
  feat_sort <- combine_vectors(features)
  for(i in 1:ncol(coeff)){
    result[i] <- sum(feat_sort * coeff[,i][-(nrow(coeff))]) +
      as.numeric(coeff[,i][(nrow(coeff))])
  }
  return(result)
}

prediction(features[237:239,], df_coeff)

RW <- function(features, coeff, start, end, lag){
  h <- start:end
  result <- as.data.frame(matrix(0, length(t), ncol(features)))
  error <- as.data.frame(matrix(0, length(t), ncol(features)))
  for(i in 1:length(start:end)){
    result[i,] <- prediction(features[(h[i]-lag):(h[i]-1),], coeff) 
    error[i,] <- rev(features[h[i],]) - result[i,]
  }
  df <- list('Wahrer Wert' = df_features[h,], 'Prediction' = result,
             'Error' = error)
  
  return(df)
}

RW(df_features, df_coeff, 229, 284, 3)



#### Benchmark Model AR ####

AR_RW <- function(target, coeff, start, end, lag){
  h <- start:end
  result <- numeric(length(start:end))
  error <- numeric(length(start:end))
  for(i in 1:length(start:end)){
    feat <- target[(h[i]-1):(h[i]-lag)]
    result[i] <- sum(feat * coeff)
    error[i] <- target[h[i]] - result[i]
  }
  df <- data.frame('Wahrer Wert' = target[h], 'Prediction' = result,
                   'Error' = error)
  
  return(df)
}

AR_IP <- ar.mle(IP_train, order.max = 12)
pred_AR_IP <- AR_RW(IP_difflog, as.vector(AR_IP$ar), 229, 284, lag = AR_IP$order)
rmse_AR_IP <- Metrics::rmse(pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction)

AR_Zins <- ar.mle(Zins_train, order.max =12)
pred_AR_Zins <- AR_RW(Zins, as.vector(AR_Zins$ar), 229, 284, lag = AR_Zins$order)
rmse_AR_Zins <- Metrics::rmse(pred_AR_Zins$Wahrer.Wert, pred_AR_Zins$Prediction)

AR_Arbeit <- ar.mle(Arbeit_train, order.max =12)
pred_AR_Arbeit <- AR_RW(Arbeit, as.vector(AR_Arbeit$ar), 229, 284, lag = AR_Arbeit$order)
rmse_AR_Arbeit <- Metrics::rmse(pred_AR_Arbeit$Wahrer.Wert, pred_AR_Arbeit$Prediction)

AR_VPI <- ar.mle(VPI_train, order.max = 1)
pred_AR_VPI <- AR_RW(VPI, as.vector(AR_VPI$ar), 229, 284, lag = 1)
rmse_AR_VPI <- Metrics::rmse(pred_AR_VPI$Wahrer.Wert, pred_AR_VPI$Prediction)


# plots plots plots

ggplot(pred_AR_IP, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_Zins, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_Arbeit, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_VPI, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')
#### Modell ohne Indexe ####

# Anordnung der Features in df reversed zu den Koeffizienten um diese dann mit
# combine_vectors in die gleiche Ordnung wie die coeffs von VAR zu bringen
df_features <- data.frame(VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

lag <- as.numeric( VARselect(df_features, 24, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten
est <- VAR(ts.union(IP_train, Zins_train,
                    Arbeit_train, VPI_train), p = lag)
summary(est)
df_coeff <- data.frame(summary(est)$varresult$IP_train$coefficients[,1],
                       summary(est)$varresult$Zins_train$coefficients[,1],
                       summary(est)$varresult$Arbeit_train$coefficients[,1],
                       summary(est)$varresult$VPI_train$coefficients[,1])

pred <- RW(df_features, df_coeff, 229, 284, 3)

rmse_pred_IP <- Metrics::rmse(pred$`Wahrer Wert`$IP_difflog, pred$Prediction$V1)
rmse_pred_Zins <- Metrics::rmse(pred$`Wahrer Wert`$Zins, pred$Prediction$V2)
rmse_pred_Arbeit <- Metrics::rmse(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3)
rmse_pred_VPI <- Metrics::rmse(pred$`Wahrer Wert`$VPI, pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = pred$`Wahrer Wert`$IP_difflog,
                  Prediction = pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = pred$`Wahrer Wert`$Zins,
                  Prediction = pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = pred$`Wahrer Wert`$Arbeit,
                  Prediction = pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = pred$`Wahrer Wert`$VPI,
                  Prediction = pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))



#### Modell mit IPI ####

IPI_df_features <- data.frame(IPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

IPI_lag <- as.numeric( VARselect(IPI_df_features, 24, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten
IPI_est <- VAR(ts.union(IP_train, Zins_train,
                        Arbeit_train, VPI_train, IPI_train), p = IPI_lag)
summary(IPI_est)
IPI_df_coeff <- data.frame(summary(IPI_est)$varresult$IP_train$coefficients[,1],
                           summary(IPI_est)$varresult$Zins_train$coefficients[,1],
                           summary(IPI_est)$varresult$Arbeit_train$coefficients[,1],
                           summary(IPI_est)$varresult$VPI_train$coefficients[,1],
                           summary(IPI_est)$varresult$IPI_train$coefficients[,1])

IPI_pred <- RW(IPI_df_features, IPI_df_coeff, 229, 284, 3)

rmse_IPI_pred_IP <- Metrics::rmse(IPI_pred$`Wahrer Wert`$IP_difflog, IPI_pred$Prediction$V1)
rmse_IPI_pred_Zins <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Zins, IPI_pred$Prediction$V2)
rmse_IPI_pred_Arbeit <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Arbeit, IPI_pred$Prediction$V3)
rmse_IPI_pred_VPI <- Metrics::rmse(IPI_pred$`Wahrer Wert`$VPI, IPI_pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$IP_difflog,
                  Prediction = IPI_pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$Zins,
                  Prediction = IPI_pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$Arbeit,
                  Prediction = IPI_pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$VPI,
                  Prediction = IPI_pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))







#### Modell mit UPI ####

UPI_df_features <- data.frame(UPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

UPI_lag <- as.numeric( VARselect(UPI_df_features, 24, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten
UPI_est <- VAR(ts.union(IP_train, Zins_train,
                        Arbeit_train, VPI_train, UPI_train), p = UPI_lag)
summary(UPI_est)
UPI_df_coeff <- data.frame(summary(UPI_est)$varresult$IP_train$coefficients[,1],
                           summary(UPI_est)$varresult$Zins_train$coefficients[,1],
                           summary(UPI_est)$varresult$Arbeit_train$coefficients[,1],
                           summary(UPI_est)$varresult$VPI_train$coefficients[,1],
                           summary(UPI_est)$varresult$UPI_train$coefficients[,1])

UPI_pred <- RW(UPI_df_features, UPI_df_coeff, 229, 284, 3)

rmse_UPI_pred_IP <- Metrics::rmse(UPI_pred$`Wahrer Wert`$IP_difflog, UPI_pred$Prediction$V1)
rmse_UPI_pred_Zins <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Zins, UPI_pred$Prediction$V2)
rmse_UPI_pred_Arbeit <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Arbeit, UPI_pred$Prediction$V3)
rmse_UPI_pred_VPI <- Metrics::rmse(UPI_pred$`Wahrer Wert`$VPI, UPI_pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = UPI_pred$`Wahrer Wert`$IP_difflog,
                  Prediction = UPI_pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = UPI_pred$`Wahrer Wert`$Zins,
                  Prediction = UPI_pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = UPI_pred$`Wahrer Wert`$Arbeit,
                  Prediction = UPI_pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = UPI_pred$`Wahrer Wert`$VPI,
                  Prediction = UPI_pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))




#### Modell mit EPU ####

EPU_df_features <- data.frame(EPU, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

EPU_lag <- as.numeric( VARselect(EPU_df_features, 24, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten
EPU_est <- VAR(ts.union(IP_train, Zins_train,
                        Arbeit_train, VPI_train, EPU_train), p = EPU_lag)
summary(EPU_est)
EPU_df_coeff <- data.frame(summary(EPU_est)$varresult$IP_train$coefficients[,1],
                           summary(EPU_est)$varresult$Zins_train$coefficients[,1],
                           summary(EPU_est)$varresult$Arbeit_train$coefficients[,1],
                           summary(EPU_est)$varresult$VPI_train$coefficients[,1],
                           summary(EPU_est)$varresult$EPU_train$coefficients[,1])

EPU_pred <- RW(EPU_df_features, EPU_df_coeff, 229, 284, EPU_lag)

rmse_EPU_pred_IP <- Metrics::rmse(EPU_pred$`Wahrer Wert`$IP_difflog, EPU_pred$Prediction$V1)
rmse_EPU_pred_Zins <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Zins, EPU_pred$Prediction$V2)
rmse_EPU_pred_Arbeit <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Arbeit, EPU_pred$Prediction$V3)
rmse_EPU_pred_VPI <- Metrics::rmse(EPU_pred$`Wahrer Wert`$VPI, EPU_pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$IP_difflog,
                  Prediction = IPI_pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$Zins,
                  Prediction = IPI_pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$Arbeit,
                  Prediction = IPI_pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = IPI_pred$`Wahrer Wert`$VPI,
                  Prediction = IPI_pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))





#### IPI Modell mit Subindexe #### 

## basic modell und hinzufÃ¼gen von selbst ausgewÃ¤hlten sub-Indexen  ##

subIPI_df_features <- data.frame(subIPI$Topic.2..News,
                                 subIPI$Topic.1..Central.Banks,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subIPI_lag <- as.numeric( VARselect(subIPI_df_features, 12, type = "const")$selection[3])

# SchÃ¤tzung der Koeffizienten
subIPI_est <- VAR(ts.union('IP_train' = ts(IP_train), 
                           'Zins_train' = ts(Zins_train),
                           'Arbeit_train' = ts(Arbeit_train),
                           'VPI_train' = ts(VPI_train),
                           'Central.Banks' = ts(subIPI_train$Topic.1..Central.Banks),
                           'News' = ts(subIPI_train$Topic.2..News)), p = subIPI_lag)
summary(subIPI_est)

subIPI_df_coeff <- data.frame(summary(subIPI_est)$varresult$IP_train$coefficients[,1],
                              summary(subIPI_est)$varresult$Zins_train$coefficients[,1],
                              summary(subIPI_est)$varresult$Arbeit_train$coefficients[,1],
                              summary(subIPI_est)$varresult$VPI_train$coefficients[,1],
                              summary(subIPI_est)$varresult$Central.Banks$coefficients[,1],
                              summary(subIPI_est)$varresult$News$coefficients[,1])

subIPI_pred <- RW(subIPI_df_features, subIPI_df_coeff, 229, 284, 3)

rmse_subIPI_pred_IP <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$IP_difflog, subIPI_pred$Prediction$V1)
rmse_subIPI_pred_Zins <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Zins, subIPI_pred$Prediction$V2)
rmse_subIPI_pred_Arbeit <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Arbeit, subIPI_pred$Prediction$V3)
rmse_subIPI_pred_VPI <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$VPI, subIPI_pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = subIPI_pred$`Wahrer Wert`$IP_difflog,
                  Prediction = subIPI_pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subIPI_pred$`Wahrer Wert`$Zins,
                  Prediction = subIPI_pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subIPI_pred$`Wahrer Wert`$Arbeit,
                  Prediction = subIPI_pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subIPI_pred$`Wahrer Wert`$VPI,
                  Prediction = subIPI_pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))




# ### sub-UPI ###


## basic modell und hinzufÃ¼gen von selbst ausgewÃ¤hlten sub-Indexen  ##

subUPI_df_features <- data.frame(subUPI$Topic.13..German.Economy,
                                 subUPI$Topic.12..Central.banks,
                                 subUPI$Topic.10..Financial.Markets.II,
                                 subUPI$Topic.2..EU.Conflicts,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subUPI_lag <- as.numeric( VARselect(subUPI_df_features, 12, type = "const")$selection[3])

# SchÃ¤tzung der Koeffizienten
subUPI_est <- VAR(ts.union('IP_train' = ts(IP_train), 
                           'Zins_train' = ts(Zins_train),
                           'Arbeit_train' = ts(Arbeit_train),
                           'VPI_train' = ts(VPI_train),
                           'EU Conflicts' = ts(subUPI_train$Topic.2..EU.Conflicts),
                           'Financial Markets' = ts(subUPI_train$Topic.10..Financial.Markets.II),
                           'Central Banks' = ts(subUPI_train$Topic.12..Central.banks),
                           'German Economy' = ts(subUPI_train$Topic.13..German.Economy))[-c(1,2),], p = subUPI_lag)
summary(subUPI_est)

subUPI_df_coeff <- data.frame(summary(subUPI_est)$varresult$IP_train$coefficients[,1],
                              summary(subUPI_est)$varresult$Zins_train$coefficients[,1],
                              summary(subUPI_est)$varresult$Arbeit_train$coefficients[,1],
                              summary(subUPI_est)$varresult$VPI_train$coefficients[,1],
                              summary(subUPI_est)$varresult$EU.Conflicts$coefficients[,1],
                              summary(subUPI_est)$varresult$Financial.Markets$coefficients[,1],
                              summary(subUPI_est)$varresult$Central.Banks$coefficients[,1],
                              summary(subUPI_est)$varresult$German.Economy$coefficients[,1])

subUPI_pred <- RW(subUPI_df_features, subUPI_df_coeff, 229, 284, subUPI_lag)

rmse_subUPI_pred_IP <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$IP_difflog, subUPI_pred$Prediction$V1)
rmse_subUPI_pred_Zins <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Zins, subUPI_pred$Prediction$V2)
rmse_subUPI_pred_Arbeit <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Arbeit, subUPI_pred$Prediction$V3)
rmse_subUPI_pred_VPI <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$VPI, subUPI_pred$Prediction$V4)

# 95% Intervall
temp$covres[1,1]

# plots plots plots

ggplot(data.frame(Wahr = subUPI_pred$`Wahrer Wert`$IP_difflog,
                  Prediction = subUPI_pred$Prediction$V1), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subUPI_pred$`Wahrer Wert`$Zins,
                  Prediction = subUPI_pred$Prediction$V2), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subUPI_pred$`Wahrer Wert`$Arbeit,
                  Prediction = subUPI_pred$Prediction$V3), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))

ggplot(data.frame(Wahr = subUPI_pred$`Wahrer Wert`$VPI,
                  Prediction = subUPI_pred$Prediction$V4), aes(x = 1:56)) +
  geom_line(aes(y = Wahr, color = 'darkred')) +
  geom_line(aes(y = Prediction, color = 'steelblue'))




# Granger KausalitÃ¤t


grangertest(Zins~Arbeit, order = 3) # nope
grangertest(Zins~IP_difflog, order = 3) # nope
grangertest(Zins~VPI, order = 3) # nope
grangertest(Zins~IPI, order = 3) # yep
grangertest(Zins~UPI, order = 3) # nope
grangertest(Zins~EPU, order = 3) # nope

grangertest(Arbeit~Zins, order = 3) # nope
grangertest(Arbeit~IP_difflog, order = 3) # nope
grangertest(Arbeit~VPI, order = 3) # nope
grangertest(Arbeit~IPI, order = 3) # nope
grangertest(Arbeit~UPI, order = 3) # nope
grangertest(Arbeit~EPU, order = 3) # nope

grangertest(IP_difflog~Zins, order = 3) # yep
grangertest(IP_difflog~Arbeit, order = 3) # nope
grangertest(IP_difflog~VPI, order = 3) # nope
grangertest(IP_difflog~IPI, order = 3) # knapp nope
grangertest(IP_difflog~UPI, order = 3) # yep
grangertest(IP_difflog~EPU, order = 3) # yep

grangertest(VPI~Zins, order = 3) # nope
grangertest(VPI~Arbeit, order = 3) # nope
grangertest(VPI~IP_difflog, order = 3) # nope
grangertest(VPI~IPI, order = 3) # yep
grangertest(VPI~UPI, order = 3) # nope
grangertest(VPI~EPU, order = 3) # knapp nope 


causality(est, cause = c("Zins_train", "Arbeit_train", "VPI_train"))
causality(est, cause = c("IP_train", "Arbeit_train", "VPI_train"))
causality(est, cause = c("IP_train", "Zins_train", "VPI_train"))
causality(est, cause = c("IP_train", "Zins_train", "Arbeit_train"))

library(bruceR)
granger_causality(est)


measured_rmse_error <- data.frame('AR model' =
                                    c(rmse_AR_IP, rmse_AR_Zins,
                                      rmse_AR_Arbeit, rmse_AR_VPI),
                                  'basic model' =
                                    c(rmse_pred_IP, rmse_pred_Zins,
                                      rmse_pred_Arbeit, rmse_pred_VPI),
                                  'IPI model' =
                                    c(rmse_IPI_pred_IP, rmse_IPI_pred_Zins,
                                      rmse_IPI_pred_Arbeit, rmse_IPI_pred_VPI), 
                                  'UPI model' =
                                    c(rmse_UPI_pred_IP, rmse_UPI_pred_Zins,
                                      rmse_UPI_pred_Arbeit, rmse_UPI_pred_VPI),
                                  'EPU model' =
                                    c(rmse_EPU_pred_IP, rmse_EPU_pred_Zins,
                                      rmse_EPU_pred_Arbeit, rmse_EPU_pred_VPI),
                                  'subIPI model' =
                                    c(rmse_subIPI_pred_IP, rmse_subIPI_pred_Zins,
                                      rmse_subIPI_pred_Arbeit, rmse_subIPI_pred_VPI),
                                  'subUPI model' =
                                    c(rmse_subUPI_pred_IP, rmse_subUPI_pred_Zins,
                                      rmse_subUPI_pred_Arbeit, rmse_subUPI_pred_VPI),
                                  row.names = c("IP", "Zins", "Arbeit", "VPI"))



####
# Likelihood Ratio Test zum Testen des Goodness of Fit zwischen dem
# basic model und denn komplexeren Modellen mit den Indexen ####

# IP

lrtest(est$varresult$IP_train, IPI_est$varresult$IP_train) # nope
lrtest(est$varresult$IP_train, UPI_est$varresult$IP_train) # nope 
lrtest(est$varresult$IP_train, EPU_est$varresult$IP_train) # nope
lrtest(est$varresult$IP_train, subIPI_est$varresult$IP_train) # nope
lrtest(est$varresult$IP_train, subUPI_est$varresult$IP_train) # yep

# Zins

lrtest(est$varresult$Zins_train, IPI_est$varresult$Zins_train) # knapp nope
lrtest(est$varresult$Zins_train, UPI_est$varresult$Zins_train) # yep
lrtest(est$varresult$Zins_train, EPU_est$varresult$Zins_train) # yep
lrtest(est$varresult$Zins_train, subIPI_est$varresult$Zins_train) # yep
lrtest(est$varresult$Zins_train, subUPI_est$varresult$Zins_train) # yep

# Arbeit

lrtest(est$varresult$Arbeit_train, IPI_est$varresult$Arbeit_train) # nope
lrtest(est$varresult$Arbeit_train, UPI_est$varresult$Arbeit_train) # nope
lrtest(est$varresult$Arbeit_train, EPU_est$varresult$Arbeit_train) # nope
lrtest(est$varresult$Arbeit_train, subIPI_est$varresult$Arbeit_train) # knapp nope
lrtest(est$varresult$Arbeit_train, subUPI_est$varresult$Arbeit_train) # yep 

# VPI

lrtest(est$varresult$VPI_train, IPI_est$varresult$VPI_train) # nope
lrtest(est$varresult$VPI_train, UPI_est$varresult$VPI_train) # nope 
lrtest(est$varresult$VPI_train, EPU_est$varresult$VPI_train) # nope
lrtest(est$varresult$VPI_train, subIPI_est$varresult$VPI_train) # nope
lrtest(est$varresult$VPI_train, subUPI_est$varresult$VPI_train) # yep












#### 95%- Konfidenzintervall ####

# Erstellen der Koeffizientenmatrizen A_i

initialize_A <- function(coeff, lag){
  list_coeff <- list()
  for(i in 1:lag){
    A <- diag(ncol(coeff))
    for(j in 1:ncol(coeff)){
      A[j,] <- coeff[,j][(1+(i-1)*ncol(coeff)):(i*ncol(coeff))]
    }
    list_coeff[[i]] <- A
  }
  return(list_coeff)
}

initialize_A(df_coeff, 3)

# Rekusive Berechnung der Phi_i

# Gerade nur möglich für j <= lag (soll aber nicht so sein)
phi_rek <- function(list_coeff, lag){
  if(lag == 0){
    return(diag(ncol(list_coeff[[1]])))
  }
  
  result_phi <- matrix(0, ncol(list_coeff[[1]]),
                       ncol(list_coeff[[1]]))
  
  for(i in 1:lag){
    result_phi <- result_phi + phi_rek(list_coeff, lag-i) %*% list_coeff[[i]] 
  }
  return(result_phi)
}
phi_rek(initialize_A(df_coeff, 3), 1)


# Erstellung der (K x T) Matrix Y
# Y <- t(rev(df_features[-c(1:lag),]))
# Y_originial <-  t(rev(df_features))   # Y plus die ersten lag-Beobachtungen 
# welche benötigt werden zum prognostizieren
# der ersten lag-Elemente in Y


initialize_Z_t <- function(features, t, lag) { 
  Y = t(rev(features)) # Y plus die ersten lag Beobachtungen 
  # welche benötigt werden zum prognostizieren
  # der ersten lag Elemente in Y
  
  k <- nrow(Y)  # Anzahl der Variablen
  
  # Verzögerte Werte von t bis t-p+1
  lagged_y <- as.vector(Y[, t:(t - lag + 1)])
  
  # Z_t als (kp+1) x 1 Matrix erstellen
  Z_t <- matrix(c(1, lagged_y), nrow = k * lag + 1, ncol = 1)
  
  return(Z_t)
}

initialize_Z_t(df_features, 280, 3)

# Einzelne Z_t als col in eine Matrix
initialze_Z <- function(features, lag){
  Z <- matrix(0, ncol(features)*lag+1, nrow(features)-lag)
  for(i in 1:(nrow(features)-lag)){
    Z[,i] <- initialize_Z_t(features, i-1+lag, lag)
  }
  return(Z)
}

initialze_Z(df_features[1:228,], 3)

# Mit Z und Y die Covariance Matrix Sigma_u schätzen

predict_covar <- function(features, lag, max_index) {
  Y <- t(rev(features[1:max_index,]))[,-c(1:lag)]
  Z <- initialze_Z(features[1:max_index,], lag)
  K <- nrow(Y)
  # Berechnung der Matrixprodukte
  YY <- Y %*% t(Y)  # Y * Y'
  YZ <- Y %*% t(Z)  # Y * Z'
  ZZ <- Z %*% t(Z)  # Z * Z'
  
  # Invertierung von ZZ'
  ZZ_inv <- solve(ZZ)
  
  # Berechnung von \hat{\Sigma}_u
  Sigma_u_hat <- (1 / (T - K * lag - 1)) * (YY - YZ %*% ZZ_inv %*% t(YZ))
  
  return(Sigma_u_hat)
}

predict_covar(df_features, 3, 228)

# Berechnung von Sigma_y(h)

predict_sigma_y <- function(features, h, lag, max_index){
  T <- max_index-lag
  K <- ncol(features)
  sigma_u_hat <- predict_covar(features, lag, max_index)
  if(h == 1){
    result <- ((T + K*lag + 1)/T) * sigma_u_hat
    return(result)
  }
  if(h == 2){
    A_1 <- initialize_A(df_coeff, lag)[[1]] # Phi_1 ist gleich A_i
    sigma_y <- sigma_u_hat + A_1 %*% sigma_u_hat %*% t(A_i)
  }
}

predict_sigma_y(df_features, 1, 3, 228)


