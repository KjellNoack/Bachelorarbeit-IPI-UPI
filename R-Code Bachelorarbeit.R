
library(readxl) # einlesen von csv Dateien
library(vars) # für VAR Analysen
library(ggplot2) # zum plotten
library(dplyr)
library(Metrics) # für mse etc.
library(lmtest) # Für Granger Kausalität und statistische Tests
library(tseries) # zum testen von Stationarität durch adf
library(NlinTS) # zum testen von Stationarität durch ad
library(olsrr) # Für Variablen Selection (Forword etc.)


IP_original <- read.delim("~/TU Dortmund/Bachelorarbeit/Industrieproduktion.txt", header=FALSE) # Ab 12.2004 bis 08.2024
IP_original <- IP_original[,-2]
IP_difflog <- diff(log(IP_original))
IP_train <- ts(IP_difflog[1:180])

Zins_original <- read.delim("~/TU Dortmund/Bachelorarbeit/Geldmarktzinssatz.txt", header=FALSE, na.strings="") # Ab 01.2005 bis 08.2024
Zins <- Zins_original$V1
Zins_train <- ts(Zins[1:180])


Arbeit_original <- read.table("~/TU Dortmund/Bachelorarbeit/Arbeitslosenquote.txt", quote="\"", comment.char="") # Ab 01.2005 bis 08.2024
Arbeit <- Arbeit_original$V1
Arbeit_train <- ts(Arbeit[1:180])

VPI_original <- read.table("~/TU Dortmund/Bachelorarbeit/VPI.txt", quote="\"", comment.char="") # Ab 01.2005 bis 08.2024
VPI <- VPI_original$V1
VPI_train <- ts(VPI[1:180])


IPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_data_08_2024.csv", sep = ",")  
IPI <- IPI_original$ipi[-c(1:48)]
IPI <- as.numeric(IPI)
IPI_train <- ts(IPI[1:180])

subIPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_subdata_08_2024.csv", sep = ",")
subIPI <- data.frame(apply(subIPI_original[-c(1:48), 2:11], 2, as.numeric))
subIPI_train <- subIPI[1:180, ]

UPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_data_10_2024.csv", sep = ",") 
UPI <- UPI_original$upi[-c(1:48, 285,286)]
UPI <- as.numeric(UPI)
UPI_train <- ts(UPI[1:180]) 

subUPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_subdata_10_2024.csv", sep = ",") 
subUPI <- data.frame(apply(subUPI_original[-c(1:48, 285, 286), 2:15], 2, as.numeric))
subUPI_train <- subUPI[1:180, ]

EPU_original <- read_excel("TU Dortmund/Bachelorarbeit/Europe_Policy_Uncertainty_Data.xlsx")
EPU <- EPU_original$Germany_News_Index[-c(1:216, 453, 454)] # Ab 01.2005 bis 08.2024
EPU_train <- ts(EPU[1:180])



#### Visualisierung der Zeitreihen

'df_features <- data.frame(
  time = rep(c(rep(1:12, 19), 1:8),4),
  values = c(Arbeit, VPI, IP, Zins),
  features = factor(rep(1:4, each = 236),
                    labels = c("Arbeit", "VPI", "IP", "Zins"))
)'


temp_Arbeit <- data.frame(Arbeit = Arbeit, x = 1:236)
ggplot(temp_Arbeit, aes(x = x, y = Arbeit)) +
   geom_line()

temp_VPI <- data.frame(VPI = VPI, x = 1:236)
ggplot(temp_VPI, aes(x = x, y = VPI)) +
  geom_line()

temp_IP <- data.frame(IP = IP, x = 1:236)
ggplot(temp_IP, aes(x = x, y = IP)) +
  geom_line()
temp_IP_difflog <- data.frame(IP_difflog = IP_difflog, x = 1:236)
ggplot(temp_IP_difflog, aes(x = x, y = IP_difflog)) + 
  geom_line()

temp_Zins <- data.frame(Zins = Zins, x = 1:236)
ggplot(temp_Zins, aes(x = x, y = Zins)) +
  geom_line()



plot_IPI <- data.frame(IPI = IPI, x = 1:236)
ggplot(plot_IPI, aes(x = x, y = IPI)) +
  geom_line()

plot_UPI <- data.frame(UPI = UPI, x = 1:236)
ggplot(plot_UPI, aes(x = x, y = UPI)) +
  geom_line()

plot_EPU <- data.frame(EPU = EPU, x = 1:236)
ggplot(plot_EPU, aes(x = x, y = EPU)) +
  geom_line()

  



# Test auf Stationarität

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

# AIC und BIC berechnen für unterschiedliche lags
min_BIC <- function(...){
  var <- ts.union(...)
  aic <- numeric(24)
  bic <- numeric(24)
  for(i in 1:24){
    aic[i] <- AIC(VAR(var, p = i))
    bic[i] <- BIC(VAR(var, p = i))
  }
  print(data.frame(AIC = aic, BIC = bic))
}

# Features in die Reihenfolge nach Lags umwandeln
combine_vectors <- function(...){
  features <- list(...)
  result <- rev(as.vector(t(data.frame(features))))
  return(result)
}

#temp <- combine_vectors(IP_difflog[168:182], Zins[168:182],
#                         Arbeit[168:182], VPI[168:182])

prediction <- function(features, coeff){
  result <- sum(features * unlist(coeff)[-(length(features) + 1)]) +
    as.numeric(unlist(coeff)[length(features) + 1])
  
  return(result)
}



# Erstellung von rolling Window Funktion

RW <- function(target, features, coeff, start, end, lag){
  h <- start:end
  result <- numeric(length(start:end))
  error <- numeric(length(start:end))
  for(i in 1:length(start:end)){
    feat_sort <- combine_vectors(features[(h[i]-lag):(h[i]-1),])
    result[i] <- prediction(feat_sort, coeff) 
    error[i] <- target[h[i]] - result[i]
  }
  df <- data.frame('Wahrer Wert' = target[h], 'Prediction' = result,
                   'Error' = error)
  
  return(df)
}



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
pred_AR_IP <- AR_RW(IP_difflog, as.vector(AR_IP$ar), 181, 236, lag = AR_IP$order)
rmse_AR_IP <- rmse(pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction)

AR_Zins <- ar.mle(Zins_train, order.max =12)
pred_AR_Zins <- AR_RW(Zins, as.vector(AR_Zins$ar), 181, 236, lag = AR_Zins$order)
rmse_AR_Zins <- rmse(pred_AR_Zins$Wahrer.Wert, pred_AR_Zins$Prediction)

AR_Arbeit <- ar.mle(Arbeit_train, order.max =12)
pred_AR_Arbeit <- AR_RW(Arbeit, as.vector(AR_Arbeit$ar), 181, 236, lag = AR_Arbeit$order)
rmse_AR_Arbeit <- rmse(pred_AR_Arbeit$Wahrer.Wert, pred_AR_Arbeit$Prediction)

AR_VPI <- ar.mle(VPI_train, order.max =12)
pred_AR_VPI <- AR_RW(VPI, as.vector(AR_VPI$ar), 181, 236, lag = AR_VPI$order)
rmse_AR_VPI <- rmse(pred_AR_VPI$Wahrer.Wert, pred_AR_VPI$Prediction)

#### Modell ohne Indexe ####

# Anordnung der Features in df reversed zu den Koeffizienten um diese dann mit
# combine_vectors in die gleiche Ordnung wie die coeffs von VAR zu bringen
df_features <- data.frame(VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

#VARselect(ts.union(IP_train, Zins_train,
#                   Arbeit_train, VPI_train), lag.max = 24)


lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train)
#lag <- which(lag_df$BIC == min(lag_df$BIC))
lag <- which(lag_df$AIC == min(lag_df$AIC))
# Schätzung der Koeffizienten
est <- VAR(ts.union(IP_train, Zins_train,
                    Arbeit_train, VPI_train), p = 2)
summary(est)


pred_IP <- RW(df_features$IP_difflog, df_features,
                est$varresult$IP_train[1], 181, 236, lag = lag)
pred_IP
mean_pred_IP <- mean(abs(pred_IP$Error))
rmse_pred_IP <- rmse(pred_IP$Wahrer.Wert, pred_IP$Prediction)

pred_Zins <- RW(df_features$Zins, df_features,
           est$varresult$Zins_train[1], 181, 236, lag = lag)
pred_Zins
mean_pred_Zins <- mean(abs(pred_Zins$Error))
rmse_pred_Zins <- rmse(pred_Zins$Wahrer.Wert, pred_Zins$Prediction)

pred_Arbeit <- RW(df_features$Arbeit, df_features,
                est$varresult$Arbeit_train[1], 181, 236, lag = lag)
pred_Arbeit
mean_pred_Arbeit <- mean(abs(pred_Arbeit$Error))
rmse_pred_Arbeit <- rmse(pred_Arbeit$Wahrer.Wert, pred_Arbeit$Prediction)

pred_VPI <- RW(df_features$VPI, df_features,
                est$varresult$VPI_train[1], 181, 236, lag = 2)
pred_VPI
mean_pred_VPI <- mean(abs(pred_VPI$Error))
rmse_pred_VPI <- rmse(pred_VPI$Wahrer.Wert, pred_VPI$Prediction)

# plots plots plots

ggplot(pred_IP, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_Zins, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_Arbeit, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_VPI, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')



#### Modell mit IPI ####

IPI_df_features <- data.frame(IPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

IPI_lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train, IPI_train)
# IPI_lag <- which(IPI_lag_df$BIC == min(IPI_lag_df$BIC))
IPI_lag <- which(IPI_lag_df$AIC == min(IPI_lag_df$AIC))

# Schätzung der Koeffizienten
IPI_est <- VAR(ts.union(IP_train, Zins_train,
                    Arbeit_train, VPI_train, IPI_train), p = IPI_lag)
summary(IPI_est)


IPI_pred_IP <- RW(IPI_df_features$IP_difflog, IPI_df_features, 
              IPI_est$varresult$IP_train[1], 181, 236, lag = IPI_lag)
IPI_pred_IP
mean_IPI_pred_IP <- mean(abs(IPI_pred_IP$Error))
rmse_IPI_pred_IP <- rmse(IPI_pred_IP$Wahrer.Wert, IPI_pred_IP$Prediction)

IPI_pred_Zins <- RW(IPI_df_features$Zins, IPI_df_features,
                    IPI_est$varresult$Zins_train[1], 181, 236, lag = IPI_lag)
IPI_pred_Zins
mean_IPI_pred_Zins <- mean(abs(IPI_pred_Zins$Error))
rmse_IPI_pred_Zins <- rmse(IPI_pred_Zins$Wahrer.Wert, IPI_pred_Zins$Prediction)

IPI_pred_Arbeit <- RW(IPI_df_features$Arbeit, IPI_df_features,
                      IPI_est$varresult$Arbeit_train[1], 181, 236, lag = IPI_lag)
IPI_pred_Arbeit
mean_IPI_pred_Arbeit <- mean(abs(IPI_pred_Arbeit$Error))
rmse_IPI_pred_Arbeit <- rmse(IPI_pred_Arbeit$Wahrer.Wert, IPI_pred_Arbeit$Prediction)

IPI_pred_VPI <- RW(IPI_df_features$VPI, IPI_df_features,
                   IPI_est$varresult$VPI_train[1], 181, 236, lag = IPI_lag)
IPI_pred_VPI
mean_IPI_pred_VPI <- mean(abs(IPI_pred_VPI$Error))
rmse_IPI_pred_VPI <- rmse(IPI_pred_VPI$Wahrer.Wert, IPI_pred_VPI$Prediction)


# plots plots plots

ggplot(IPI_pred_IP, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(IPI_pred_Zins, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(IPI_pred_Arbeit, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(IPI_pred_VPI, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')






#### Modell mit UPI ####

UPI_df_features <- data.frame(UPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

UPI_lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train, UPI_train)
#UPI_lag <- which(UPI_lag_df$BIC == min(UPI_lag_df$BIC))
UPI_lag <- which(UPI_lag_df$AIC == min(UPI_lag_df$AIC))

# Schätzung der Koeffizienten
UPI_est <- VAR(ts.union(IP_train, Zins_train,
                        Arbeit_train, VPI_train, UPI_train), p = UPI_lag)
summary(UPI_est)


UPI_pred_IP <- RW(UPI_df_features$IP_difflog, UPI_df_features, 
                  UPI_est$varresult$IP_train[1], 181, 236, lag = UPI_lag)
UPI_pred_IP
mean_UPI_pred_IP <- mean(abs(UPI_pred_IP$Error))
rmse_UPI_pred_IP <- rmse(UPI_pred_IP$Wahrer.Wert, UPI_pred_IP$Prediction)

UPI_pred_Zins <- RW(UPI_df_features$Zins, UPI_df_features,
                    UPI_est$varresult$Zins_train[1], 181, 236, lag = UPI_lag)
UPI_pred_Zins
mean_UPI_pred_Zins <- mean(abs(UPI_pred_Zins$Error))
rmse_UPI_pred_Zins <- rmse(UPI_pred_Zins$Wahrer.Wert, UPI_pred_Zins$Prediction)

UPI_pred_Arbeit <- RW(UPI_df_features$Arbeit, UPI_df_features,
                      UPI_est$varresult$Arbeit_train[1], 181, 236, lag = UPI_lag)
UPI_pred_Arbeit
mean_UPI_pred_Arbeit <- mean(abs(UPI_pred_Arbeit$Error), na.rm = TRUE)
rmse_UPI_pred_Arbeit <- rmse(UPI_pred_Arbeit$Wahrer.Wert, UPI_pred_Arbeit$Prediction)

UPI_pred_VPI <- RW(UPI_df_features$VPI, UPI_df_features,
                   UPI_est$varresult$VPI_train[1], 181, 236, lag = UPI_lag)
UPI_pred_VPI
mean_UPI_pred_VPI <- mean(abs(UPI_pred_VPI$Error), na.rm = TRUE)
rmse_UPI_pred_VPI <- rmse(UPI_pred_VPI$Wahrer.Wert, UPI_pred_VPI$Prediction)


# plots plots plots

ggplot(UPI_pred_IP, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(UPI_pred_Zins, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(UPI_pred_Arbeit, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(UPI_pred_VPI, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')






#### Modell mit EPU ####

EPU_df_features <- data.frame(EPU, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

EPU_lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train, EPU_train)
#EPU_lag <- which(EPU_lag_df$BIC == min(EPU_lag_df$BIC))
EPU_lag <- which(EPU_lag_df$AIC == min(EPU_lag_df$AIC))

# Schätzung der Koeffizienten
EPU_est <- VAR(ts.union(IP_train, Zins_train,
                        Arbeit_train, VPI_train, EPU_train), p = EPU_lag)
summary(EPU_est)


EPU_pred_IP <- RW(EPU_df_features$IP_difflog, EPU_df_features, 
                  EPU_est$varresult$IP_train[1], 181, 236, lag = EPU_lag)
EPU_pred_IP
mean_EPU_pred_IP <- mean(abs(EPU_pred_IP$Error))
rmse_EPU_pred_IP <- rmse(EPU_pred_IP$Wahrer.Wert, EPU_pred_IP$Prediction)

EPU_pred_Zins <- RW(EPU_df_features$Zins, EPU_df_features,
                    EPU_est$varresult$Zins_train[1], 181, 236, lag = EPU_lag)
EPU_pred_Zins
mean_EPU_pred_Zins <- mean(abs(EPU_pred_Zins$Error), na.rm = TRUE)
rmse_EPU_pred_Zins <- rmse(EPU_pred_Zins$Wahrer.Wert, EPU_pred_Zins$Prediction)

EPU_pred_Arbeit <- RW(EPU_df_features$Arbeit, EPU_df_features,
                      EPU_est$varresult$Arbeit_train[1], 181, 236, lag = EPU_lag)
EPU_pred_Arbeit
mean_EPU_pred_Arbeit <- mean(abs(EPU_pred_Arbeit$Error), na.rm = TRUE)
rmse_EPU_pred_Arbeit <- rmse(EPU_pred_Arbeit$Wahrer.Wert, EPU_pred_Arbeit$Prediction)

EPU_pred_VPI <- RW(EPU_df_features$VPI, EPU_df_features,
                   EPU_est$varresult$VPI_train[1], 181, 236, lag = EPU_lag)
EPU_pred_VPI
mean_EPU_pred_VPI <- mean(abs(EPU_pred_VPI$Error), na.rm = TRUE)
rmse_EPU_pred_VPI <- rmse(EPU_pred_VPI$Wahrer.Wert, EPU_pred_VPI$Prediction)


# plots plots plots

ggplot(EPU_pred_IP, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')
 
ggplot(EPU_pred_Zins, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(EPU_pred_Arbeit, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(EPU_pred_VPI, aes(x = 1:54)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')






# #### sub-IPI Indizes ####
# 
# ## Lag = 1 und forward variablen selektion ##
# 
# # Auslasssen des lettzten Datenpunktes um gleiche Länge zu erhalten wie
# # basic model um lrtest durchzuführen
# subIPI_model <- VAR(ts.union(ts(IP_train[1:179]), ts(Zins_train[1:179]),
#                              ts(Arbeit_train[1:179]), ts(VPI_train[1:179]),
#                              ts(subIPI_train[1:179, ])), p = 1)
# 
# 
# ## IP_difflog
# 
# # Forward variable selection
# subIPI_est_IP <- ols_step_forward_aic(subIPI_model$varresult$ts.IP_train.1.179..,
#                                       include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                   "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# subIPI_est_IP$model[1]
# 
# subIPI_df_features_IP <- data.frame(IP_difflog, Zins, Arbeit, 
#                                     VPI, subIPI$Topic.6..Miscellaneous,
#                                     subIPI$Topic.2..News)
# 
# 
# subIPI_pred_IP <- RW(subIPI_df_features_IP$IP_difflog, subIPI_df_features_IP, 
#                      rev(unlist(subIPI_est_IP$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subIPI_pred_IP
# mean_subIPI_pred_IP <- mean(abs(subIPI_pred_IP$Error))
# rmse_subIPI_pred_IP <- rmse(subIPI_pred_IP$Wahrer.Wert, subIPI_pred_IP$Prediction)
# 
# 
# ## Zins
# 
# # Forward variable selection
# subIPI_est_Zins <- ols_step_forward_aic(subIPI_model$varresult$ts.Zins_train.1.179..,
#                                       include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                   "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subIPI_est_Zins$model[1]
# 
# subIPI_df_features_Zins <- data.frame(IP_difflog, Zins, Arbeit, VPI,
#                                  subIPI$Topic.7..Financial.Markets,
#                                  subIPI$Topic.6..Miscellaneous,
#                                  subIPI$Topic.8..Companies,
#                                  subIPI$Topic.10..Raw.Materials,
#                                  subIPI$Topic.9..German.Politics,
#                                  subIPI$Topic.4..Eurozone)
# 
# 
# subIPI_pred_Zins <- RW(Zins, subIPI_df_features_Zins, 
#                      rev(unlist(subIPI_est_Zins$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subIPI_pred_Zins
# mean_subIPI_pred_Zins <- mean(abs(subIPI_pred_Zins$Error))
# rmse_subIPI_pred_Zins <- rmse(subIPI_pred_Zins$Wahrer.Wert, subIPI_pred_Zins$Prediction)
# 
# 
# 
# ## Arbeit
# 
# subIPI_est_Arbeit <- ols_step_forward_aic(subIPI_model$varresult$ts.Arbeit_train.1.179..,
#                                         include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                     "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subIPI_est_Arbeit$model[1]
# 
# subIPI_df_features_Arbeit <- data.frame(IP_difflog, Zins, Arbeit, VPI,
#                                       subIPI$Topic.6..Miscellaneous,
#                                       subIPI$Topic.8..Companies,
#                                       subIPI$Topic.5..Private.Investment,
#                                       subIPI$Topic.3..Emerging.Markets)
# 
# 
# subIPI_pred_Arbeit <- RW(Arbeit, subIPI_df_features_Arbeit, 
#                        rev(unlist(subIPI_est_Arbeit$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subIPI_pred_Arbeit
# mean_subIPI_pred_Arbeit <- mean(abs(subIPI_pred_Arbeit$Error))
# rmse_subIPI_pred_Arbeit <- rmse(subIPI_pred_Arbeit$Wahrer.Wert, subIPI_pred_Arbeit$Prediction)
# 
# 
# 
# ## VPI
# 
# subIPI_est_VPI <- ols_step_forward_aic(subIPI_model$varresult$ts.VPI_train.1.179..,
#                                        include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                    "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subIPI_est_VPI$model[1]
# 
# subIPI_df_features_VPI <- data.frame(VPI)
# 
# 
# subIPI_pred_VPI <- RW(VPI, subIPI_df_features_VPI, 
#                          rev(unlist(subIPI_est_VPI$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subIPI_pred_VPI
# mean_subIPI_pred_VPI <- mean(abs(subIPI_pred_VPI$Error))
# rmse_subIPI_pred_VPI <- rmse(subIPI_pred_VPI$Wahrer.Wert, subIPI_pred_VPI$Prediction)
# 
# 
# # plots plots plots
# 
# ggplot(subIPI_pred_IP, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subIPI_pred_Zins, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subIPI_pred_Arbeit, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subIPI_pred_VPI, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# 


## basic modell und hinzufügen von selbst ausgewählten sub-Indexen  ##

subIPI_df_features <- data.frame(subIPI$Topic.2..News,
                                 subIPI$Topic.1..Central.Banks,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subIPI_lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train,
                      subIPI_train$Topic.1..Central.Banks,
                      subIPI_train$Topic.2..News)
#subIPI_lag <- which(subIPI_lag_df$BIC == min(subIPI_lag_df$BIC))
subIPI_lag <- which(subIPI_lag_df$AIC == min(subIPI_lag_df$AIC))

# Schätzung der Koeffizienten
subIPI_est <- VAR(ts.union('IP_train' = ts(IP_train), 
                             'Zins_train' = ts(Zins_train),
                             'Arbeit_train' = ts(Arbeit_train),
                             'VPI_train' = ts(VPI_train),
                             'Central.Banks' = ts(subIPI_train$Topic.1..Central.Banks),
                             'News' = ts(subIPI_train$Topic.2..News)), p = subIPI_lag)
summary(subIPI_est)


subIPI_pred_IP <- RW(subIPI_df_features$IP_difflog, subIPI_df_features, 
                     subIPI_est$varresult$IP_train[1], 181, 236, lag = subIPI_lag)
subIPI_pred_IP
rmse_subIPI_pred_IP <- rmse(subIPI_pred_IP$Wahrer.Wert, subIPI_pred_IP$Prediction)

subIPI_pred_Zins <- RW(subIPI_df_features$Zins, subIPI_df_features,
                       subIPI_est$varresult$Zins_train[1], 181, 236, lag = subIPI_lag)
subIPI_pred_Zins
rmse_subIPI_pred_Zins <- rmse(subIPI_pred_Zins$Wahrer.Wert, subIPI_pred_Zins$Prediction)

subIPI_pred_Arbeit <- RW(subIPI_df_features$Arbeit, subIPI_df_features,
                         subIPI_est$varresult$Arbeit_train[1], 181, 236, lag = subIPI_lag)
subIPI_pred_Arbeit
rmse_subIPI_pred_Arbeit <- rmse(subIPI_pred_Arbeit$Wahrer.Wert, subIPI_pred_Arbeit$Prediction)

subIPI_pred_VPI <- RW(subIPI_df_features$VPI, subIPI_df_features,
                      subIPI_est$varresult$VPI_train[1], 181, 236, lag = subIPI_lag)
subIPI_pred_VPI
rmse_subIPI_pred_VPI <- rmse(subIPI_pred_VPI$Wahrer.Wert, subIPI_pred_VPI$Prediction)


# plots plots plots

ggplot(subIPI_pred_IP, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_Zins, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_Arbeit, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_VPI, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')




# ### sub-UPI ###
# 
# ## Lag = 1 und forward variablen selektion ##
# 
# # AUslasssen des lettzten Datenpunktes um gleiche Länge zu erhalten wie
# # basic model um lrtest durchzuführen
# subUPI_model <- VAR(ts.union('IP_train' = ts(IP_train[1:179]), 
#                              'Zins_train' = ts(Zins_train[1:179]),
#                              'Arbeit_train' = ts(Arbeit_train[1:179]),
#                              'VPI_train' = ts(VPI_train[1:179]),
#                              ts(subUPI_train[1:179, ])), p = 1)
# 
# library(olsrr)
# 
# ## IP_difflog
# 
# # Forward variable selection
# subUPI_est_IP <- ols_step_forward_aic(subUPI_model$varresult$IP_train,
#                                       include = c("IP_train.l1", "Zins_train.l1",
#                                                  "Arbeit_train.l1", "VPI_train.l1"))
# subUPI_est_IP$model[1]
# 
# subUPI_df_features_IP <- data.frame(IP_difflog, Zins, Arbeit, 
#                                     VPI, subUPI$Topic.9..Miscellaneous)
# 
# 
# subUPI_pred_IP <- RW(IP_difflog, subUPI_df_features_IP, 
#                      rev(unlist(subUPI_est_IP$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subUPI_pred_IP
# mean_subUPI_pred_IP <- mean(abs(subUPI_pred_IP$Error))
# rmse_subUPI_pred_IP <- rmse(subUPI_pred_IP$Wahrer.Wert, subUPI_pred_IP$Prediction)
# 
# 
# ## Zins
# 
# # Forward variable selection
# subUPI_est_Zins <- ols_step_forward_aic(subUPI_model$varresult$ts.Zins_train.1.179..,
#                                          include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                      "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subUPI_est_Zins$model[1]
# 
# subUPI_df_features_Zins <- data.frame(IP_difflog, Zins, Arbeit, VPI,
#                                       subUPI$Topic.4..Companies...Markets,
#                                       subUPI$Topic.3..Energy...Climate.Change.Mitigation)
# 
# 
# subUPI_pred_Zins <- RW(Zins, subUPI_df_features_Zins, 
#                        rev(unlist(subUPI_est_Zins$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subUPI_pred_Zins
# mean_subUPI_pred_Zins <- mean(abs(subUPI_pred_Zins$Error))
# rmse_subUPI_pred_Zins <- rmse(subUPI_pred_Zins$Wahrer.Wert, subUPI_pred_Zins$Prediction)
# 
# 
# 
# ## Arbeit
# 
# subUPI_est_Arbeit <- ols_step_forward_aic(subUPI_model$varresult$ts.Arbeit_train.1.179..,
#                                           include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                       "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subUPI_est_Arbeit$model[1]
# 
# subUPI_df_features_Arbeit <- data.frame(IP_difflog, Zins, Arbeit, VPI,
#                                         subUPI$Topic.4..Companies...Markets,
#                                         subUPI$Topic.1..Corporate.Culture)
# 
# 
# subUPI_pred_Arbeit <- RW(Arbeit, subUPI_df_features_Arbeit, 
#                          rev(unlist(subUPI_est_Arbeit$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subUPI_pred_Arbeit
# mean_subUPI_pred_Arbeit <- mean(abs(subUPI_pred_Arbeit$Error))
# rmse_subUPI_pred_Arbeit <- rmse(subUPI_pred_Arbeit$Wahrer.Wert, subUPI_pred_Arbeit$Prediction)
# 
# 
# 
# ## VPI
# 
# subUPI_est_VPI <- ols_step_forward_aic(subUPI_model$varresult$ts.VPI_train.1.179..,
#                                        include = c("ts.IP_train.1.179...l1", "ts.Zins_train.1.179...l1",
#                                                    "ts.Arbeit_train.1.179...l1", "ts.VPI_train.1.179...l1"))
# 
# subUPI_est_VPI$model[1] # Will am liebsten nur VPI drin haben
# 
# subUPI_df_features_VPI <- data.frame(IP_difflog, Zins, Arbeit, VPI,
#                                      subUPI$Topic.5..Geopolitics)
# 
# 
# subUPI_pred_VPI <- RW(VPI, subUPI_df_features_VPI, 
#                       rev(unlist(subUPI_est_VPI$model[1])), 181, 236, lag = 1) # reverse weil in RW Fukntion davon ausgeggangen wird dass der Intercept der letzte Koeffizient ist
# subUPI_pred_VPI
# mean_subUPI_pred_VPI <- mean(abs(subUPI_pred_VPI$Error))
# rmse_subUPI_pred_VPI <- rmse(subUPI_pred_VPI$Wahrer.Wert, subUPI_pred_VPI$Prediction)
# 
# 
# # plots plots plots
# 
# ggplot(subUPI_pred_IP, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subUPI_pred_Zins, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subUPI_pred_Arbeit, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')
# 
# ggplot(subUPI_pred_VPI, aes(x = 1:56)) +
#   geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
#   geom_line(aes(y = Prediction), color = 'steelblue')

## basic modell und hinzufügen von selbst ausgewählten sub-Indexen  ##

subUPI_df_features <- data.frame(subUPI$Topic.13..German.Economy,
                                 subUPI$Topic.12..Central.banks,
                                 subUPI$Topic.10..Financial.Markets.II,
                                 subUPI$Topic.2..EU.Conflicts,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subUPI_lag_df <- min_BIC(IP_train, Zins_train, Arbeit_train, VPI_train,
                         subUPI_train$Topic.2..EU.Conflicts,
                         subUPI_train$Topic.10..Financial.Markets.II,
                         subUPI_train$Topic.12..Central.banks,
                         subUPI_train$Topic.13..German.Economy)
#subUPI_lag <- which(subIPI_lag_df$BIC == min(subIPI_lag_df$BIC))
subUPI_lag <- which(subIPI_lag_df$AIC == min(subIPI_lag_df$AIC))

# Schätzung der Koeffizienten
subUPI_est <- VAR(ts.union('IP_train' = ts(IP_train), 
                           'Zins_train' = ts(Zins_train),
                           'Arbeit_train' = ts(Arbeit_train),
                           'VPI_train' = ts(VPI_train),
                           'EU Conflicts' = ts(subUPI_train$Topic.2..EU.Conflicts),
                           'Financial Markets' = ts(subUPI_train$Topic.10..Financial.Markets.II),
                           'Central Banks' = ts(subUPI_train$Topic.12..Central.banks),
                           'German Economy' = ts(subUPI_train$Topic.13..German.Economy)), p = 1)
summary(subUPI_est)


subUPI_pred_IP <- RW(subUPI_df_features$IP_difflog, subUPI_df_features, 
                     subUPI_est$varresult$IP_train[1], 181, 236, lag = 1)
subUPI_pred_IP
rmse_subUPI_pred_IP <- rmse(subIPI_pred_IP$Wahrer.Wert, subIPI_pred_IP$Prediction)

subUPI_pred_Zins <- RW(subUPI_df_features$Zins, subUPI_df_features,
                       subUPI_est$varresult$Zins_train[1], 181, 236, lag = 1)
subUPI_pred_Zins
rmse_subUPI_pred_Zins <- rmse(subUPI_pred_Zins$Wahrer.Wert, subUPI_pred_Zins$Prediction)

subUPI_pred_Arbeit <- RW(subUPI_df_features$Arbeit, subUPI_df_features,
                         subUPI_est$varresult$Arbeit_train[1], 181, 236, lag = 1)
subUPI_pred_Arbeit
rmse_subUPI_pred_Arbeit <- rmse(subUPI_pred_Arbeit$Wahrer.Wert, subUPI_pred_Arbeit$Prediction)

subUPI_pred_VPI <- RW(subUPI_df_features$VPI, subUPI_df_features,
                      subUPI_est$varresult$VPI_train[1], 181, 236, lag = 1)
subUPI_pred_VPI
rmse_subUPI_pred_VPI <- rmse(subUPI_pred_VPI$Wahrer.Wert, subUPI_pred_VPI$Prediction)


# plots plots plots

ggplot(subIPI_pred_IP, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_Zins, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_Arbeit, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(subIPI_pred_VPI, aes(x = 1:56)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')




# Granger Kausalität


grangertest(Zins~Arbeit, order = 2) # nope
grangertest(Zins~IP_difflog, order = 2) # nope
grangertest(Zins~VPI, order = 2) # nope
grangertest(Zins~IPI, order = 2) # yep
grangertest(Zins~UPI, order = 2) # nope
grangertest(Zins~EPU, order = 2) # nope

grangertest(Arbeit~Zins, order = 2) # nope
grangertest(Arbeit~IP_difflog, order = 2) # nope
grangertest(Arbeit~VPI, order = 2) # nope
grangertest(Arbeit~IPI, order = 2) # nope
grangertest(Arbeit~UPI, order = 2) # nope
grangertest(Arbeit~EPU, order = 2) # nope

grangertest(IP_difflog~Zins, order = 2) # yep
grangertest(IP_difflog~Arbeit, order = 2) # nope
grangertest(IP_difflog~VPI, order = 2) # nope
grangertest(IP_difflog~IPI, order = 2) # knapp nope
grangertest(IP_difflog~UPI, order = 2) # yep
grangertest(IP_difflog~EPU, order = 2) # yep

grangertest(VPI~Zins, order = 2) # nope
grangertest(VPI~Arbeit, order = 2) # nope
grangertest(VPI~IP_difflog, order = 2) # nope
grangertest(VPI~IPI, order = 2) # yep
grangertest(VPI~UPI, order = 2) # nope
grangertest(VPI~EPU, order = 2) # knapp nope 


causality(est, cause = c("Zins_train", "Arbeit_train", "VPI_train"))
causality(est, cause = c("IP_train", "Arbeit_train", "VPI_train"))
causality(est, cause = c("IP_train", "Zins_train", "VPI_train"))
causality(est, cause = c("IP_train", "Zins_train", "Arbeit_train"))

library(bruceR)
granger_causality(est)



measured_mean_error <- data.frame('basic model' =
                                    c(mean_pred_IP, mean_pred_Zins,
                                      mean_pred_Arbeit, mean_pred_VPI),
                                  'IPI model' =
                                    c(mean_IPI_pred_IP, mean_IPI_pred_Zins,
                                      mean_IPI_pred_Arbeit, mean_IPI_pred_VPI), 
                                  'UPI model' =
                                    c(mean_UPI_pred_IP, mean_UPI_pred_Zins,
                                      mean_UPI_pred_Arbeit, mean_UPI_pred_VPI),
                                  'EPU model' =
                                    c(mean_EPU_pred_IP, mean_EPU_pred_Zins,
                                      mean_EPU_pred_Arbeit, mean_EPU_pred_VPI),
                                  'subIPI VS model' =
                                    c(mean_subIPI_pred_IP, mean_subIPI_pred_Zins,
                                      mean_subIPI_pred_Arbeit, mean_subIPI_pred_VPI),
                                  'subUPI VS model' =
                                    c(mean_subUPI_pred_IP, mean_subUPI_pred_Zins,
                                      mean_subUPI_pred_Arbeit, mean_subUPI_pred_VPI),
                                  row.names = c("IP", "Zins", "Arbeit", "VPI"))

# measured_mse_error <- data.frame('basic model' =
#                                     c(mse_pred_IP, mse_pred_Zins,
#                                       mse_pred_Arbeit, mse_pred_VPI),
#                                   'IPI model' =
#                                     c(mse_IPI_pred_IP, mse_IPI_pred_Zins,
#                                       mse_IPI_pred_Arbeit, mse_IPI_pred_VPI),
#                                   'UPI model' =
#                                     c(mse_UPI_pred_IP, mse_UPI_pred_Zins,
#                                       mse_UPI_pred_Arbeit, mse_UPI_pred_VPI),
#                                   'EPU model' =
#                                     c(mse_EPU_pred_IP, mse_EPU_pred_Zins,
#                                       mse_EPU_pred_Arbeit, mse_EPU_pred_VPI),
#                                   'subIPI VS model' =
#                                     c(mse_subIPI_pred_IP, mse_subIPI_pred_Zins,
#                                       mse_subIPI_pred_Arbeit, mse_subIPI_pred_VPI),
#                                   'subUPI VS model' =
#                                     c(mse_subUPI_pred_IP, mse_subUPI_pred_Zins,
#                                       mse_subUPI_pred_Arbeit, mse_subUPI_pred_VPI),
#                                   row.names = c("IP", "Zins", "Arbeit", "VPI"))

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
lrtest(est$varresult$IP_train, subIPI_est_IP$model) # yep
lrtest(est$varresult$IP_train, subUPI_est_IP$model) # yep

# Zins

lrtest(est$varresult$Zins_train, IPI_est$varresult$Zins_train)
lrtest(est$varresult$Zins_train, UPI_est$varresult$Zins_train)
lrtest(est$varresult$Zins_train, EPU_est$varresult$Zins_train)
lrtest(est$varresult$Zins_train, subIPI_est_Zins$model)
lrtest(est$varresult$Zins_train, subUPI_est_Zins$model)

# Arbeit

lrtest(est$varresult$Arbeit_train, IPI_est$varresult$Arbeit_train)
lrtest(est$varresult$Arbeit_train, UPI_est$varresult$Arbeit_train)
lrtest(est$varresult$Arbeit_train, EPU_est$varresult$Arbeit_train)
lrtest(est$varresult$Arbeit_train, subIPI_est_Arbeit$model)
lrtest(est$varresult$Arbeit_train, subUPI_est_Arbeit$model)

# VPI

lrtest(est$varresult$VPI_train, IPI_est$varresult$VPI_train) # nope
lrtest(est$varresult$VPI_train, UPI_est$varresult$VPI_train) # nope 
lrtest(est$varresult$VPI_train, EPU_est$varresult$VPI_train) # nope
lrtest(est$varresult$VPI_train, subIPI_est_VPI$model) # nope
lrtest(est$varresult$VPI_train, subUPI_est_VPI$model) # nope
