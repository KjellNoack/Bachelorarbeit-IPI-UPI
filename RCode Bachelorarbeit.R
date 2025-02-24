
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
IP_original <- IP_original$V1[1:287]
IP_original <- as.numeric(gsub(",", ".", IP_original))
IP_difflog <- diff(log(IP_original)) # 01.01-10.24
IP_train <- ts(IP_difflog[1:228])

Zins_original <- read.delim("~/TU Dortmund/Bachelorarbeit/Geldmarktzinssatz.txt", header=FALSE, na.strings="") # Ab 01.2001 bis 08.2024
Zins <- Zins_original$V1[1:286]
Zins_train <- ts(Zins[1:228])


Arbeit_original <- read.table("~/TU Dortmund/Bachelorarbeit/Arbeitslosenquote.txt", quote="\"", comment.char="") # Ab 01.2001 bis 08.2024
Arbeit <- Arbeit_original$V1[1:286] # 01.01-10.24
Arbeit_train <- ts(Arbeit[1:228])

VPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/Verbraucherpreisindex.csv", sep = ";", header = FALSE) 
VPI <- gsub(",", ".",  VPI_original$V5[-c(1:126, 415:418)])
VPI[c(106,170)] <- 0 
VPI <- as.numeric(VPI)[1:286] # 01.01-10.24
VPI_train <- ts(VPI[1:228])


IPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_data_10_24.csv", sep = ",")  
IPI <- IPI_original$ipi
IPI <- as.numeric(IPI)
IPI_train <- ts(IPI[1:228])

subIPI_original <- read.csv2("TU Dortmund/Bachelorarbeit/ipi_topics_10_24.csv", sep = ",")
subIPI <- data.frame(apply(subIPI_original[, 2:11], 2, as.numeric))
subIPI_train <- subIPI[1:228, ]

UPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_data_12_24.csv", sep = ",") 
UPI <- UPI_original$upi[1:286]
UPI <- as.numeric(UPI)
UPI_train <- ts(UPI[1:228]) 

subUPI_original <- read.csv2("~/TU Dortmund/Bachelorarbeit/upi_topics_12_24.csv", sep = ",") 
subUPI <- data.frame(apply(subUPI_original[1:286, 2:15], 2, as.numeric))
subUPI_train <- subUPI[1:228, ]

EPU_original <- read_excel("TU Dortmund/Bachelorarbeit/Europe_Policy_Uncertainty_Data.xlsx")
EPU <- EPU_original$Germany_News_Index[-c(1:168)]
EPU <- EPU[1:286]  # Ab 01.2001 bis 08.2024
EPU_train <- ts(EPU[1:228])



#### Visualisierung der Zeitreihen

'df_features <- data.frame(
  time = rep(c(rep(1:12, 19), 1:8),4),
  values = c(Arbeit, VPI, IP, Zins),
  features = factor(rep(1:4, each = 236),
                    labels = c("Arbeit", "VPI", "IP", "Zins"))
)'


temp_Arbeit <- data.frame(Arbeit = Arbeit, x = 1:286)
ggplot(temp_Arbeit, aes(x = x, y = Arbeit)) +
  geom_line()

temp_VPI <- data.frame(VPI = VPI, x = 1:286)
ggplot(temp_VPI, aes(x = x, y = VPI)) +
  geom_line()

temp_IP_difflog <- data.frame(IP_difflog = IP_difflog, x = 1:286)
ggplot(temp_IP_difflog, aes(x = x, y = IP_difflog)) + 
  geom_line()

temp_Zins <- data.frame(Zins = Zins, x = 1:286)
ggplot(temp_Zins, aes(x = x, y = Zins)) +
  geom_line()



plot_IPI <- data.frame(IPI = IPI, Jahr = 1:286)
ggplot(plot_IPI, aes(x = Jahr, y = IPI)) +
  geom_line() +
  scale_x_continuous(breaks = c(seq(0, 300,36)),
                     labels = c(seq(2001, 2025, 3))) +
  scale_y_continuous(breaks = seq(0,0.15,0.03))  +
  ggtitle("IPI-Index für Deutschland")

plot_UPI <- data.frame(UPI = as.numeric(UPI_original$upi),
                       Jahr = 1:length(UPI_original$upi))
ggplot(plot_UPI, aes(x = Jahr, y = UPI)) +
  geom_line() +
  scale_x_continuous(breaks = c(seq(0, 300,36)),
                     labels = c(seq(2001, 2025, 3))) +
  scale_y_continuous(breaks = seq(0,0.06,0.01))  +
  ggtitle("UPI-Index für Deutschland")

plot_EPU <- data.frame(EPU = EPU_original$Germany_News_Index[73:457],
                       Jahr = 1:length(EPU_original$Germany_News_Index[73:457]))
ggplot(plot_EPU, aes(x = Jahr, y = EPU)) +
  geom_line() +
  scale_x_continuous(breaks = c(seq(0, 360,36), 384),
                     labels = c(seq(1993, 2025, 3),2025)) + 
  scale_y_continuous(breaks = seq(0,1000,100)) +
  ggtitle("EPU-Index für Deutschland",
          subtitle = "(100 = Mittelwert bis 2011)")





# Test auf StationaritÃ¤t

# Dickey-Fuller Test 

df_test_Zins <- ur.df(Zins, "none", 1)
summary(df_test_Zins)
df_test_Zins_train <- ur.df(Zins_train, "none", 1)
summary(df_test_Zins_train)

df_test_Arbeit <- ur.df(Arbeit, "none", 1)
summary(df_test_Arbeit)
df_test_Arbeit_train <- ur.df(Arbeit_train, "none", 1)
summary(df_test_Arbeit_train)

df_test_IP <- ur.df(IP_difflog, "none", 1)
summary(df_test_IP)
df_test_IP_train <- ur.df(IP_train, "none", 1)
summary(df_test_IP_train)

df_test_VPI <- ur.df(VPI, "none", 1)
summary(df_test_VPI)
df_test_VPI_train <- ur.df(VPI_train, "none", 1)
summary(df_test_VPI_train)

df_test_IPI <- ur.df(IPI, "none", 1)
summary(df_test_IPI)
df_test_IPI_train <- ur.df(IPI_train, "none", 1)
summary(df_test_IPI_train)

df_test_UPI <- ur.df(UPI, "none", 1)
summary(df_test_UPI)
df_test_UPI_train <- ur.df(UPI_train, "none", 1)
summary(df_test_UPI_train)

df_test_EPU <- ur.df(EPU, "none", 1)
summary(df_test_EPU)
df_test_EPU_train <- ur.df(EPU_train, "none", 1)
summary(df_test_EPU_train)



#### Ablauf einer Prediction ####


# Features in die Reihenfolge nach Lags umwandeln
combine_vectors <- function(...){
  features <- list(...)
  result <- rev(as.vector(t(data.frame(features))))
  return(result)
}

combine_vectors(IP_difflog[168:182], Zins[168:182],
                Arbeit[168:182], VPI[168:182])





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


# Erstellung von rolling Funktionen

EW_model_building <- function(features, start, lag) {
  
  results <- list()
  n <- nrow(features)
  h <- start:n
  
  for (i in 1:length(h)) {
    train_data <- features[1:h[i], ]
    model <- VAR(train_data, p = lag)
    results[[i]] <- model
  }
  
  return(results)
}

RW <- function(features, start, lag){
  n <- nrow(features)
  h <- start:n
  k <- ncol(features)
  result <- as.data.frame(matrix(0, length(t), ncol(features)))
  error <- as.data.frame(matrix(0, length(t), ncol(features)))
  coeff <- as.data.frame(matrix(0, (k*lag+1), ncol(features)))
  models <- EW_model_building(rev(features), start, lag)
  for(i in 1:length(start:n)){
    for(j in 1:k){
      coeff[,j] <- models[[i]]$varresult[[j]][1]
    }
    result[i,] <- prediction(features[(h[i]-lag):(h[i]-1),], coeff) 
    error[i,] <- rev(features[h[i],]) - result[i,]
  }
  df <- list('Wahrer Wert' = rev(features)[h,], 'Prediction' = result,
             'Error' = error)
  
  return(df)
}
RW(df_features, 229, 3)





#### Benchmark Model AR ####

AR_RW <- function(target, start, lag){
  n <- length(target)
  h <- start:n
  result <- numeric(length(h))
  error <- numeric(length(h))
  for(i in 1:length(start:n)){
    temp_model <- lm(X1~., data = data.frame(embed(target[1:h[i]], lag+1)))
    feat <- c(1, target[(h[i]-1):(h[i]-lag)])
    result[i] <- sum(feat * as.vector(temp_model$coefficients))
    error[i] <- target[h[i]] - result[i]
  }
  df <- data.frame('Wahrer Wert' = target[h], 'Prediction' = result,
                   'Error' = error)
  
  return(df)
}
AR_RW(IP_difflog, 229, lag = 4)

AR_RW_2step <- function(target, pred, coeff, start, end, lag){
  h <- start:end
  result <- numeric(length(start:end))
  error <- numeric(length(start:end))
  for(i in 1:length(start:end)){
    feat <- c(1, pred[h[i]-1], target[(h[i]-2):(h[i]-lag)])
    result[i] <- sum(feat * coeff)
    error[i] <- target[h[i]] - result[i]
  }
  df <- data.frame('Wahrer Wert' = target[h], 'Prediction' = result,
                   'Error' = error)
  
  return(df)
}
AR_RW_2step(pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction,
         as.vector(AR_IP$coefficients), 12, 56, 11)


pred_AR_IP <- AR_RW(IP_difflog, 229, lag = 3)
rmse_AR_IP <- Metrics::rmse(pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction)
rmse_AR_IP_c <- Metrics::rmse(pred_AR_IP$Wahrer.Wert[1:24], pred_AR_IP$Prediction[1:24])
rmse_AR_IP_postc <- Metrics::rmse(pred_AR_IP$Wahrer.Wert[25:56], pred_AR_IP$Prediction[25:56])
pred_AR_IP_2step <- AR_RW_2step(pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction, as.vector(AR_IP$coefficients), 12, 56, 11)
rmse_AR_IP_2step <-  Metrics::rmse(pred_AR_IP_2step$Wahrer.Wert, pred_AR_IP_2step$Prediction)

pred_AR_Zins <- AR_RW(Zins, 229, lag = 3)
rmse_AR_Zins <- Metrics::rmse(pred_AR_Zins$Wahrer.Wert, pred_AR_Zins$Prediction)
rmse_AR_Zins_c<- Metrics::rmse(pred_AR_Zins$Wahrer.Wert[1:24], pred_AR_Zins$Prediction[1:24])
rmse_AR_Zins_postc <- Metrics::rmse(pred_AR_Zins$Wahrer.Wert[25:56], pred_AR_Zins$Prediction[25:56])
pred_AR_Zins_2step <- AR_RW_2step(pred_AR_Zins$Wahrer.Wert, pred_AR_Zins$Prediction, as.vector(AR_Zins$coefficients), 3, 56, 2)
rmse_AR_Zins_2step <-  Metrics::rmse(pred_AR_Zins_2step$Wahrer.Wert, pred_AR_Zins_2step$Prediction)

pred_AR_Arbeit <- AR_RW(Arbeit, 229, lag = 3)
rmse_AR_Arbeit <- Metrics::rmse(pred_AR_Arbeit$Wahrer.Wert, pred_AR_Arbeit$Prediction)
rmse_AR_Arbeit_c <- Metrics::rmse(pred_AR_Arbeit$Wahrer.Wert[1:24], pred_AR_Arbeit$Prediction[1:24])
rmse_AR_Arbeit_postc <- Metrics::rmse(pred_AR_Arbeit$Wahrer.Wert[25:56], pred_AR_Arbeit$Prediction[25:26])
pred_AR_Arbeit_2step <- AR_RW_2step(pred_AR_Arbeit$Wahrer.Wert, pred_AR_Arbeit$Prediction, as.vector(AR_Arbeit$coefficients), 13, 56, 12)
rmse_AR_Arbeit_2step <-  Metrics::rmse(pred_AR_Arbeit_2step$Wahrer.Wert, pred_AR_Arbeit_2step$Prediction)


pred_AR_VPI <- AR_RW(VPI, 229, lag = 3)
rmse_AR_VPI <- Metrics::rmse(pred_AR_VPI$Wahrer.Wert, pred_AR_VPI$Prediction)
rmse_AR_VPI_c <- Metrics::rmse(pred_AR_VPI$Wahrer.Wert[1:24], pred_AR_VPI$Prediction[1:24])
rmse_AR_VPI_postc <- Metrics::rmse(pred_AR_VPI$Wahrer.Wert[25:56], pred_AR_VPI$Prediction[25:56])
pred_AR_VPI_2step <- AR_RW_2step(pred_AR_VPI$Wahrer.Wert, pred_AR_VPI$Prediction, as.vector(AR_VPI$coefficients), 3, 56, 2)
rmse_AR_VPI_2step <-  Metrics::rmse(pred_AR_VPI_2step$Wahrer.Wert, pred_AR_VPI_2step$Prediction)


# plots plots plots

ggplot(pred_AR_IP, aes(x = 1:58)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_Zins, aes(x = 1:58)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_Arbeit, aes(x = 1:58)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

ggplot(pred_AR_VPI, aes(x = 1:58)) +
  geom_line(aes(y = Wahrer.Wert), color = 'darkred') +
  geom_line(aes(y = Prediction), color = 'steelblue')

#### Modell ohne Indexe ####

# Anordnung der Features in df reversed zu den Koeffizienten um diese dann mit
# combine_vectors in die gleiche Ordnung wie die coeffs von VAR zu bringen
df_features <- data.frame(VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

lag <- as.numeric( VARselect(df_features, 12, type = "const")$selection[3])

# Schätzungen der Modelle und Berechnung der Prediction und Fehler
pred <- RW(df_features, 229, lag)

rmse_pred_IP <- Metrics::rmse(pred$`Wahrer Wert`$IP_difflog, pred$Prediction$V1)
rmse_pred_Zins <- Metrics::rmse(pred$`Wahrer Wert`$Zins, pred$Prediction$V2)
rmse_pred_Arbeit <- Metrics::rmse(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3)
rmse_pred_VPI <- Metrics::rmse(pred$`Wahrer Wert`$VPI, pred$Prediction$V4)

# Corona Zeitraum
rmse_pred_IP_c <- Metrics::rmse(pred$`Wahrer Wert`$IP_difflog[1:24], pred$Prediction$V1[1:24])
rmse_pred_Zins_c <- Metrics::rmse(pred$`Wahrer Wert`$Zins[1:24], pred$Prediction$V2[1:24])
rmse_pred_Arbeit_c <- Metrics::rmse(pred$`Wahrer Wert`$Arbeit[1:24], pred$Prediction$V3[1:24])
rmse_pred_VPI_c <- Metrics::rmse(pred$`Wahrer Wert`$VPI[1:24], pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_pred_IP_postc <- Metrics::rmse(pred$`Wahrer Wert`$IP_difflog[25:56], pred$Prediction$V1[25:56])
rmse_pred_Zins_postc <- Metrics::rmse(pred$`Wahrer Wert`$Zins[25:56], pred$Prediction$V2[25:56])
rmse_pred_Arbeit_postc <- Metrics::rmse(pred$`Wahrer Wert`$Arbeit[25:56], pred$Prediction$V3[25:56])
rmse_pred_VPI_postc <- Metrics::rmse(pred$`Wahrer Wert`$VPI[25:56], pred$Prediction$V4[25:56])

## 2-tep vorherasage
pred_2step <- RW_2step(pred$`Wahrer Wert`, pred$Prediction, df_coeff, 4, 56, 3)

rmse_pred_IP_2step <- Metrics::rmse(pred_2step$`Wahrer Wert`$IP_difflog, pred_2step$Prediction$V1)
rmse_pred_Zins_2step <- Metrics::rmse(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2)
rmse_pred_Arbeit_2step <- Metrics::rmse(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3)
rmse_pred_VPI_2step <- Metrics::rmse(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4)



#### Modell mit IPI ####

IPI_df_features <- data.frame(IPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

IPI_lag <- as.numeric( VARselect(IPI_df_features, 12, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten

IPI_pred <- RW(IPI_df_features, 229, IPI_lag)

rmse_IPI_pred_IP <- Metrics::rmse(IPI_pred$`Wahrer Wert`$IP_difflog, IPI_pred$Prediction$V1)
rmse_IPI_pred_Zins <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Zins, IPI_pred$Prediction$V2)
rmse_IPI_pred_Arbeit <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Arbeit, IPI_pred$Prediction$V3)
rmse_IPI_pred_VPI <- Metrics::rmse(IPI_pred$`Wahrer Wert`$VPI, IPI_pred$Prediction$V4)

# Corona Zeitraum
rmse_IPI_pred_IP_c <- Metrics::rmse(IPI_pred$`Wahrer Wert`$IP_difflog[1:24], IPI_pred$Prediction$V1[1:24])
rmse_IPI_pred_Zins_c <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Zins[1:24], IPI_pred$Prediction$V2[1:24])
rmse_IPI_pred_Arbeit_c <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Arbeit[1:24], IPI_pred$Prediction$V3[1:24])
rmse_IPI_pred_VPI_c <- Metrics::rmse(IPI_pred$`Wahrer Wert`$VPI[1:24], IPI_pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_IPI_pred_IP_postc <- Metrics::rmse(IPI_pred$`Wahrer Wert`$IP_difflog[25:56], IPI_pred$Prediction$V1[25:56])
rmse_IPI_pred_Zins_postc <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Zins[25:56], IPI_pred$Prediction$V2[25:56])
rmse_IPI_pred_Arbeit_postc <- Metrics::rmse(IPI_pred$`Wahrer Wert`$Arbeit[25:56], IPI_pred$Prediction$V3[25:56])
rmse_IPI_pred_VPI_postc <- Metrics::rmse(IPI_pred$`Wahrer Wert`$VPI[25:56], IPI_pred$Prediction$V4[25:56])

## 2-step Vorhersage
IPI_pred_2step <- RW_2step(IPI_pred$`Wahrer Wert`, IPI_pred$Prediction, IPI_df_coeff, 4, 56, IPI_lag)

rmse_IPI_pred_IP_2step <- Metrics::rmse(IPI_pred_2step$`Wahrer Wert`$IP_difflog, IPI_pred_2step$Prediction$V1)
rmse_IPI_pred_Zins_2step <- Metrics::rmse(IPI_pred_2step$`Wahrer Wert`$Zins, IPI_pred_2step$Prediction$V2)
rmse_IPI_pred_Arbeit_2step <- Metrics::rmse(IPI_pred_2step$`Wahrer Wert`$Arbeit, IPI_pred_2step$Prediction$V3)
rmse_IPI_pred_VPI_2step <- Metrics::rmse(IPI_pred_2step$`Wahrer Wert`$VPI, IPI_pred_2step$Prediction$V4)


#### Modell mit UPI ####

UPI_df_features <- data.frame(UPI, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

UPI_lag <- as.numeric( VARselect(UPI_df_features, 12, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten

UPI_pred <- RW(UPI_df_features, 229, UPI_lag)

rmse_UPI_pred_IP <- Metrics::rmse(UPI_pred$`Wahrer Wert`$IP_difflog, UPI_pred$Prediction$V1)
rmse_UPI_pred_Zins <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Zins, UPI_pred$Prediction$V2)
rmse_UPI_pred_Arbeit <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Arbeit, UPI_pred$Prediction$V3)
rmse_UPI_pred_VPI <- Metrics::rmse(UPI_pred$`Wahrer Wert`$VPI, UPI_pred$Prediction$V4)

# Corona Zeitraum
rmse_UPI_pred_IP_c <- Metrics::rmse(UPI_pred$`Wahrer Wert`$IP_difflog[1:24], UPI_pred$Prediction$V1[1:24])
rmse_UPI_pred_Zins_c <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Zins[1:24], UPI_pred$Prediction$V2[1:24])
rmse_UPI_pred_Arbeit_c <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Arbeit[1:24], UPI_pred$Prediction$V3[1:24])
rmse_UPI_pred_VPI_c <- Metrics::rmse(UPI_pred$`Wahrer Wert`$VPI[1:24], UPI_pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_UPI_pred_IP_postc <- Metrics::rmse(UPI_pred$`Wahrer Wert`$IP_difflog[25:56], UPI_pred$Prediction$V1[25:56])
rmse_UPI_pred_Zins_postc <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Zins[25:56], UPI_pred$Prediction$V2[25:56])
rmse_UPI_pred_Arbeit_postc <- Metrics::rmse(UPI_pred$`Wahrer Wert`$Arbeit[25:56], UPI_pred$Prediction$V3[25:56])
rmse_UPI_pred_VPI_postc <- Metrics::rmse(UPI_pred$`Wahrer Wert`$VPI[25:56], UPI_pred$Prediction$V4[25:56])

## 2-step Vorhersage
UPI_pred_2step <- RW_2step(UPI_pred$`Wahrer Wert`, UPI_pred$Prediction, UPI_df_coeff, 4, 56, IPI_lag)

rmse_UPI_pred_IP_2step <- Metrics::rmse(UPI_pred_2step$`Wahrer Wert`$IP_difflog, UPI_pred_2step$Prediction$V1)
rmse_UPI_pred_Zins_2step <- Metrics::rmse(UPI_pred_2step$`Wahrer Wert`$Zins, UPI_pred_2step$Prediction$V2)
rmse_UPI_pred_Arbeit_2step <- Metrics::rmse(UPI_pred_2step$`Wahrer Wert`$Arbeit, UPI_pred_2step$Prediction$V3)
rmse_UPI_pred_VPI_2step <- Metrics::rmse(UPI_pred_2step$`Wahrer Wert`$VPI, UPI_pred_2step$Prediction$V4)


#### Modell mit EPU ####

EPU_df_features <- data.frame(EPU, VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

EPU_lag <- as.numeric( VARselect(EPU_df_features, 12, type = "const")$selection[3])
# SchÃ¤tzung der Koeffizienten

EPU_pred <- RW(EPU_df_features, 229, EPU_lag)

rmse_EPU_pred_IP <- Metrics::rmse(EPU_pred$`Wahrer Wert`$IP_difflog, EPU_pred$Prediction$V1)
rmse_EPU_pred_Zins <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Zins, EPU_pred$Prediction$V2)
rmse_EPU_pred_Arbeit <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Arbeit, EPU_pred$Prediction$V3)
rmse_EPU_pred_VPI <- Metrics::rmse(EPU_pred$`Wahrer Wert`$VPI, EPU_pred$Prediction$V4)


# Corona Zeitraum
rmse_EPU_pred_IP_c <- Metrics::rmse(EPU_pred$`Wahrer Wert`$IP_difflog[1:24], EPU_pred$Prediction$V1[1:24])
rmse_EPU_pred_Zins_c <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Zins[1:24], EPU_pred$Prediction$V2[1:24])
rmse_EPU_pred_Arbeit_c <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Arbeit[1:24], EPU_pred$Prediction$V3[1:24])
rmse_EPU_pred_VPI_c <- Metrics::rmse(EPU_pred$`Wahrer Wert`$VPI[1:24], EPU_pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_EPU_pred_IP_postc <- Metrics::rmse(EPU_pred$`Wahrer Wert`$IP_difflog[25:56], EPU_pred$Prediction$V1[25:56])
rmse_EPU_pred_Zins_postc <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Zins[25:56], EPU_pred$Prediction$V2[25:56])
rmse_EPU_pred_Arbeit_postc <- Metrics::rmse(EPU_pred$`Wahrer Wert`$Arbeit[25:56], EPU_pred$Prediction$V3[25:56])
rmse_EPU_pred_VPI_postc <- Metrics::rmse(EPU_pred$`Wahrer Wert`$VPI[25:56], EPU_pred$Prediction$V4[25:56])

## 2-step Vorhersage
EPU_pred_2step <- RW_2step(EPU_pred$`Wahrer Wert`, EPU_pred$Prediction, EPU_df_coeff, 4, 56, EPU_lag)

rmse_EPU_pred_IP_2step <- Metrics::rmse(EPU_pred_2step$`Wahrer Wert`$IP_difflog, EPU_pred_2step$Prediction$V1)
rmse_EPU_pred_Zins_2step <- Metrics::rmse(EPU_pred_2step$`Wahrer Wert`$Zins, EPU_pred_2step$Prediction$V2)
rmse_EPU_pred_Arbeit_2step <- Metrics::rmse(EPU_pred_2step$`Wahrer Wert`$Arbeit, EPU_pred_2step$Prediction$V3)
rmse_EPU_pred_VPI_2step <- Metrics::rmse(EPU_pred_2step$`Wahrer Wert`$VPI, EPU_pred_2step$Prediction$V4)



#### IPI Modell mit Subindexe #### 

## basic modell und hinzufÃ¼gen von selbst ausgewÃ¤hlten sub-Indexen  ##
## Ausgewählt nach den vier sub_Indexe die als "causing" Inflation    ##
## deklariert wurden im Paper.              
## 

causalIPI <- subIPI$Topic.10..Raw.Materials + 
  subIPI$Topic.9..German.Politics +
  subIPI$Topic.4..Eurozone +
  subIPI$Topic.1..Central.Banks

subIPI_df_features <- data.frame(causalIPI,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subIPI_lag <- as.numeric( VARselect(subIPI_df_features, 12, type = "const")$selection[3])

# SchÃ¤tzung der Koeffizienten

subIPI_pred <- RW(subIPI_df_features, 229, subIPI_lag)

rmse_subIPI_pred_IP <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$IP_difflog, subIPI_pred$Prediction$V1)
rmse_subIPI_pred_Zins <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Zins, subIPI_pred$Prediction$V2)
rmse_subIPI_pred_Arbeit <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Arbeit, subIPI_pred$Prediction$V3)
rmse_subIPI_pred_VPI <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$VPI, subIPI_pred$Prediction$V4)

# Corona Zeitraum
rmse_subIPI_pred_IP_c <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$IP_difflog[1:24], subIPI_pred$Prediction$V1[1:24])
rmse_subIPI_pred_Zins_c <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Zins[1:24], subIPI_pred$Prediction$V2[1:24])
rmse_subIPI_pred_Arbeit_c <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Arbeit[1:24], subIPI_pred$Prediction$V3[1:24])
rmse_subIPI_pred_VPI_c <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$VPI[1:24], subIPI_pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_subIPI_pred_IP_postc <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$IP_difflog[25:56], subIPI_pred$Prediction$V1[25:56])
rmse_subIPI_pred_Zins_postc <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Zins[25:56], subIPI_pred$Prediction$V2[25:56])
rmse_subIPI_pred_Arbeit_postc <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$Arbeit[25:56], subIPI_pred$Prediction$V3[25:56])
rmse_subIPI_pred_VPI_postc <- Metrics::rmse(subIPI_pred$`Wahrer Wert`$VPI[25:56], subIPI_pred$Prediction$V4[25:56])

## 2-step Vorhersage
subIPI_pred_2step <- RW_2step(subIPI_pred$`Wahrer Wert`, subIPI_pred$Prediction, subIPI_df_coeff, 4, 56, subIPI_lag)

rmse_subIPI_pred_IP_2step <- Metrics::rmse(subIPI_pred_2step$`Wahrer Wert`$IP_difflog, subIPI_pred_2step$Prediction$V1)
rmse_subIPI_pred_Zins_2step <- Metrics::rmse(subIPI_pred_2step$`Wahrer Wert`$Zins, subIPI_pred_2step$Prediction$V2)
rmse_subIPI_pred_Arbeit_2step <- Metrics::rmse(subIPI_pred_2step$`Wahrer Wert`$Arbeit, subIPI_pred_2step$Prediction$V3)
rmse_subIPI_pred_VPI_2step <- Metrics::rmse(subIPI_pred_2step$`Wahrer Wert`$VPI, subIPI_pred_2step$Prediction$V4)



#### sub-UPI ####

## drei Modelle, jeweils mit einer den Ober-Unterkategorien      ##

ecoUPI <- subUPI$Topic.1..Corporate.Culture +
  subUPI$Topic.3..Energy...Climate.Change.Mitigation +
  subUPI$Topic.4..Companies...Markets +
  subUPI$Topic.11..Leisure...Hospitality +
  subUPI$Topic.13..German.Economy

subUPI_df_features <- data.frame(ecoUPI,
                                 VPI, Arbeit, Zins, IP_difflog)

# Anzahl lags durch BIC herausfinden

subUPI_lag <- as.numeric( VARselect(subUPI_df_features, 12, type = "const")$selection[3])

# SchÃ¤tzung der Koeffizienten

subUPI_pred <- RW(subUPI_df_features, 229, subUPI_lag)

rmse_subUPI_pred_IP <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$IP_difflog, subUPI_pred$Prediction$V1)
rmse_subUPI_pred_Zins <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Zins, subUPI_pred$Prediction$V2)
rmse_subUPI_pred_Arbeit <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Arbeit, subUPI_pred$Prediction$V3)
rmse_subUPI_pred_VPI <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$VPI, subUPI_pred$Prediction$V4)

# Corona Zeitraum
rmse_subUPI_pred_IP_c <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$IP_difflog[1:24], subUPI_pred$Prediction$V1[1:24])
rmse_subUPI_pred_Zins_c <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Zins[1:24], subUPI_pred$Prediction$V2[1:24])
rmse_subUPI_pred_Arbeit_c <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Arbeit[1:24], subUPI_pred$Prediction$V3[1:24])
rmse_subUPI_pred_VPI_c <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$VPI[1:24], subUPI_pred$Prediction$V4[1:24])

# post-Corona Zeitraum
rmse_subUPI_pred_IP_postc <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$IP_difflog[25:56], subUPI_pred$Prediction$V1[25:56])
rmse_subUPI_pred_Zins_postc <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Zins[25:56], subUPI_pred$Prediction$V2[25:56])
rmse_subUPI_pred_Arbeit_postc <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$Arbeit[25:56], subUPI_pred$Prediction$V3[25:56])
rmse_subUPI_pred_VPI_postc <- Metrics::rmse(subUPI_pred$`Wahrer Wert`$VPI[25:56], subUPI_pred$Prediction$V4[25:56])

## 2-step Vorhersage
subUPI_pred_2step <- RW_2step(subUPI_pred$`Wahrer Wert`, subUPI_pred$Prediction, subUPI_df_coeff, 4, 56, subUPI_lag)

rmse_subUPI_pred_IP_2step <- Metrics::rmse(subUPI_pred_2step$`Wahrer Wert`$IP_difflog, subUPI_pred_2step$Prediction$V1)
rmse_subUPI_pred_Zins_2step <- Metrics::rmse(subUPI_pred_2step$`Wahrer Wert`$Zins, subUPI_pred_2step$Prediction$V2)
rmse_subUPI_pred_Arbeit_2step <- Metrics::rmse(subUPI_pred_2step$`Wahrer Wert`$Arbeit, subUPI_pred_2step$Prediction$V3)
rmse_subUPI_pred_VPI_2step <- Metrics::rmse(subUPI_pred_2step$`Wahrer Wert`$VPI, subUPI_pred_2step$Prediction$V4)


# Granger Kausalität
library(bruceR)

est <- VAR(df_features, p = lag)
IPI_est <- VAR(IPI_df_features, p = IPI_lag)
UPI_est <- VAR(UPI_df_features, p = UPI_lag)
EPU_est <- VAR(EPU_df_features, p = EPU_lag)
subIPI_est <- VAR(subIPI_df_features, p = subIPI_lag)
subUPI_est <- VAR(subUPI_df_features, p = subUPI_lag)

granger_causality(est)
granger_causality(IPI_est)
granger_causality(UPI_est)
granger_causality(EPU_est)
granger_causality(subIPI_est)
granger_causality(subUPI_est)


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

measured_rmse_error_c <- data.frame('AR model' =
                                    c(rmse_AR_IP_c, rmse_AR_Zins_c,
                                      rmse_AR_Arbeit_c, rmse_AR_VPI_c),
                                  'basic model' =
                                    c(rmse_pred_IP_c, rmse_pred_Zins_c,
                                      rmse_pred_Arbeit_c, rmse_pred_VPI_c),
                                  'IPI model' =
                                    c(rmse_IPI_pred_IP_c, rmse_IPI_pred_Zins_c,
                                      rmse_IPI_pred_Arbeit_c, rmse_IPI_pred_VPI_c), 
                                  'UPI model' =
                                    c(rmse_UPI_pred_IP_c, rmse_UPI_pred_Zins_c,
                                      rmse_UPI_pred_Arbeit_c, rmse_UPI_pred_VPI_c),
                                  'EPU model' =
                                    c(rmse_EPU_pred_IP_c, rmse_EPU_pred_Zins_c,
                                      rmse_EPU_pred_Arbeit_c, rmse_EPU_pred_VPI_c),
                                  'subIPI model' =
                                    c(rmse_subIPI_pred_IP_c, rmse_subIPI_pred_Zins_c,
                                      rmse_subIPI_pred_Arbeit_c, rmse_subIPI_pred_VPI_c),
                                  'subUPI model' =
                                    c(rmse_subUPI_pred_IP_c, rmse_subUPI_pred_Zins_c,
                                      rmse_subUPI_pred_Arbeit_c, rmse_subUPI_pred_VPI_c),
                                  row.names = c("IP", "Zins", "Arbeit", "VPI"))

measured_rmse_error_postc <- data.frame('AR model' =
                                      c(rmse_AR_IP_postc, rmse_AR_Zins_postc,
                                        rmse_AR_Arbeit_postc, rmse_AR_VPI_postc),
                                    'basic model' =
                                      c(rmse_pred_IP_postc, rmse_pred_Zins_postc,
                                        rmse_pred_Arbeit_postc, rmse_pred_VPI_postc),
                                    'IPI model' =
                                      c(rmse_IPI_pred_IP_postc, rmse_IPI_pred_Zins_postc,
                                        rmse_IPI_pred_Arbeit_postc, rmse_IPI_pred_VPI_postc), 
                                    'UPI model' =
                                      c(rmse_UPI_pred_IP_postc, rmse_UPI_pred_Zins_postc,
                                        rmse_UPI_pred_Arbeit_postc, rmse_UPI_pred_VPI_postc),
                                    'EPU model' =
                                      c(rmse_EPU_pred_IP_postc, rmse_EPU_pred_Zins_postc,
                                        rmse_EPU_pred_Arbeit_postc, rmse_EPU_pred_VPI_postc),
                                    'subIPI model' =
                                      c(rmse_subIPI_pred_IP_postc, rmse_subIPI_pred_Zins_postc,
                                        rmse_subIPI_pred_Arbeit_postc, rmse_subIPI_pred_VPI_postc),
                                    'subUPI model' =
                                      c(rmse_subUPI_pred_IP_postc, rmse_subUPI_pred_Zins_postc,
                                        rmse_subUPI_pred_Arbeit_postc, rmse_subUPI_pred_VPI_postc),
                                    row.names = c("IP", "Zins", "Arbeit", "VPI"))


measured_rmse_error_2step <- data.frame('AR model' =
                                          c(rmse_AR_IP_2step, rmse_AR_Zins_2step,
                                            rmse_AR_Arbeit_2step, rmse_AR_VPI_2step),
                                        'basic model' =
                                          c(rmse_pred_IP_2step, rmse_pred_Zins_2step,
                                            rmse_pred_Arbeit_2step, rmse_pred_VPI_2step),
                                        'IPI model' =
                                          c(rmse_IPI_pred_IP_2step, rmse_IPI_pred_Zins_2step,
                                            rmse_IPI_pred_Arbeit_2step, rmse_IPI_pred_VPI_2step), 
                                        'UPI model' =
                                          c(rmse_UPI_pred_IP_2step, rmse_UPI_pred_Zins_2step,
                                            rmse_UPI_pred_Arbeit_2step, rmse_UPI_pred_VPI_2step),
                                        'EPU model' =
                                          c(rmse_EPU_pred_IP_2step, rmse_EPU_pred_Zins_2step,
                                            rmse_EPU_pred_Arbeit_2step, rmse_EPU_pred_VPI_2step),
                                        'subIPI model' =
                                          c(rmse_subIPI_pred_IP_2step, rmse_subIPI_pred_Zins_2step,
                                            rmse_subIPI_pred_Arbeit_2step, rmse_subIPI_pred_VPI_2step),
                                        'subUPI model' =
                                          c(rmse_subUPI_pred_IP_2step, rmse_subUPI_pred_Zins_2step,
                                            rmse_subUPI_pred_Arbeit_2step, rmse_subUPI_pred_VPI_2step),
                                        row.names = c("IP", "Zins", "Arbeit", "VPI"))



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
phi_rek <- function(list_coeff, j){
  if(j == 0){
    return(diag(ncol(list_coeff[[1]])))
  }
  
  result_phi <- matrix(0, ncol(list_coeff[[1]]),
                       ncol(list_coeff[[1]]))
  
  for(i in 1:j){
    result_phi <- result_phi + phi_rek(list_coeff, j-i) %*% list_coeff[[i]] 
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
  n <- max_index-lag
  # Berechnung der Matrixprodukte
  YY <- Y %*% t(Y)  # Y * Y'
  YZ <- Y %*% t(Z)  # Y * Z'
  ZZ <- Z %*% t(Z)  # Z * Z'
  
  # Invertierung von ZZ'
  ZZ_inv <- solve(ZZ)
  
  # Berechnung von \hat{\Sigma}_u
  Sigma_u_hat <- (1 / (n - K * lag - 1)) * (YY - YZ %*% ZZ_inv %*% t(YZ))
  
  return(Sigma_u_hat)
}

predict_covar(df_features, 3, 228)


# Berechnung von Sigma_y(h)

compute_Sigma_y <- function(features, coeff, h, lag, max_index) {
  
  Sigma_u <- predict_covar(features, lag, max_index = max_index)
  Sigma_y <- matrix(0, nrow = ncol(coeff), ncol = ncol(coeff))
  Phi_list <- list()
  for(i in 0:h){
    Phi_list[[i+1]] <- phi_rek(initialize_A(coeff, 3), i)
  }
  
  for (i in 0:(h-1)) {
    Phi_i <- Phi_list[[i + 1]]  
    Sigma_y <- Sigma_y + Phi_i %*% Sigma_u %*% t(Phi_i)
  }
  
  return(Sigma_y)
}

compute_Sigma_y(df_features, df_coeff, 1, 3, 228)


# Berechnung von B

initialize_B <- function(coeff, lag){
  A_temp <- initialize_A(coeff, lag)
  v <- numeric(ncol(coeff))
  for(i in 1:ncol(coeff)){
    v[i] <- coeff[nrow(coeff), i]
  }
  B_temp <- data.frame(v, A_temp)
  
  B <- matrix(0, ncol(coeff)*lag +1, ncol(coeff)*lag +1)
  B[1,1] <- 1
  B[2:(ncol(coeff) +1), ] <- as.matrix(B_temp)
  for(i in 2:(nrow(B)-ncol(coeff))){
    B[(i+ncol(coeff)),i] <- 1
  }
  
  return(B)
}
initialize_B(df_coeff, 3)

# Berechnung von Omega

compute_Omega <- function(features, coeff, lag, h, max_index){
  
  B <- initialize_B(coeff, lag)
  Z <- initialze_Z(df_features[1:max_index,], lag)
  ZZ <- Z %*% t(Z)
  Sigma_u <- predict_covar(features, lag, max_index = max_index)

  if(h == 2){
    Phi_1 <- initialize_A(coeff, lag)[[1]]            # Phi_1 ist gleich A_i
    term1 <- sum(diag(t(B) %*% solve(ZZ) %*% B %*% t(ZZ))) * Sigma_u
    term2 <- sum(diag(t(B))) * Sigma_u %*% t(Phi_1)
    term3 <- sum(diag(B)) * Phi_1 %*% Sigma_u
    term4 <- (ncol(df_coeff)*3+1) * Phi_1 %*% Sigma_u %*% t(Phi_1)
    
    result <- term1 + term2 + term3 + term4
    return(result)
  }
}

compute_Omega(df_features, df_coeff, 3, 2, 228)

# Berechnung von Sigma_y_hat(h)

predict_sigma_y_hat <- function(features, coeff, lag, h, max_index){
  T <- max_index-lag
  K <- ncol(features)
  Sigma_u_hat <- predict_covar(features, lag, max_index)
  if(h == 1){
    Sigma_y_hat_1 <- ((T + K*lag + 1)/T) * Sigma_u_hat
    return(Sigma_y_hat_1)
  }
  else{
    Sigma_y <- compute_Sigma_y(features, coeff, h, lag, max_index)
    Omega_h <- compute_Omega(features, coeff, lag, h, max_index)
    
    return(Sigma_y)
  }
}

predict_sigma_y_hat(df_features, df_coeff, 3, 2, 228)


# Erstellung des 95% Intervalls

compute_interval <- function(features, coeff, pred, lag, h, end_index){
  t <- 229:end_index    # 229 Index für den 1.1.2020, Beginn des Test Datensatz
  result <- list()
  for(j in 1:(ncol(features))){
    result_temp <- data.frame("lower" = numeric(length(t)), 
                              "prediction" = pred$Prediction[1:length(t),j],
                              "upper" = numeric(length(t)))
    
    for(i in 1:length(t)){
      # standard diviation = sqrt von diag von MSE Matrix   
      s <- sqrt(diag(predict_sigma_y_hat(features, coeff, lag, h, t[i]-1)))[j]
      result_temp[i,1] <- pred$Prediction[i,j] - 1.96 * s
      result_temp[i,3] <- pred$Prediction[i,j] + 1.96 * s
    }
    result[[j]] <- result_temp
  }
  return(result)
}

plot_interval <- function(pred, pred_interval, i){
    p <- ggplot() +
      geom_polygon(aes(x = c(1:58, 58:1), 
                       y = c(pred_interval[[i]][,1], rev(pred_interval[[i]][,3]))),
                   alpha = 0.3, fill = 'steelblue') + 
      geom_line(aes(x = 1:58, y = unlist(pred$Prediction[i])),
                color = 'darkblue', linewidth = 0.7) +
      geom_line(aes(x = 1:58, y = unlist(pred$`Wahrer Wert`[i])), 
                color = 'red', linewidth = 0.7)
  
  return(p)
}

xlabel <- c("01.20", "07.20", "01.21", "07.21", "01.22", "07.22",
            "01.23", "07.23", "01.24", "07.24")

pred_interval <- compute_interval(df_features, df_coeff, pred, 3, 1, end_index = 286)

plot_interval(pred, pred_interval, 1) + 
  xlab("Monat")+
  ylab("Industrieproduktion") +
  ggtitle("Industrieproduktion (Basis-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 58, 6),
                     labels = xlabel)
plot_interval(pred, pred_interval, 2) + 
  xlab("Monat")+
  ylab("Geldmarktzins") +
  ggtitle("Geldmarktzins (Basis-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 58, 6),
                     labels = xlabel)
plot_interval(pred, pred_interval, 3) + 
  xlab("Jahr")+
  ylab("Arbeitslosigkeit") +
  ggtitle("Arbeitslosigkeit (Basis-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 58, 6),
                     labels = xlabel)
plot_interval(pred, pred_interval, 4) + 
  xlab("Jahr")+
  ylab("Inflation") +
  ggtitle("Inflation (Basis-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 58, 6),
                     labels = xlabel)


IPI_pred_interval <- compute_interval(IPI_df_features, IPI_df_coeff, IPI_pred, 3, 1, end_index = 286)

plot_interval(IPI_pred, IPI_pred_interval, 1) 
plot_interval(IPI_pred, IPI_pred_interval, 2)
plot_interval(IPI_pred, IPI_pred_interval, 3)
plot_interval(IPI_pred, IPI_pred_interval, 4)


UPI_pred_interval <- compute_interval(UPI_df_features, UPI_df_coeff, UPI_pred, 3, 1, end_index = 286)

plot_interval(UPI_pred, UPI_pred_interval, 1)
plot_interval(UPI_pred, UPI_pred_interval, 2)
plot_interval(UPI_pred, UPI_pred_interval, 3)
plot_interval(UPI_pred, UPI_pred_interval, 4)


EPU_pred_interval <- compute_interval(EPU_df_features, EPU_df_coeff, EPU_pred, 3, 1, end_index = 286)

plot_interval(EPU_pred, EPU_pred_interval, 1)
plot_interval(EPU_pred, EPU_pred_interval, 2)
plot_interval(EPU_pred, EPU_pred_interval, 3) + 
  xlab("Jahr") +
  ylab("Arbeitslosigkeit") +
  ggtitle("Arbeitslosigkeit (EPU-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 56, 6),
                     labels = xlabel)
plot_interval(EPU_pred, EPU_pred_interval, 4)


subIPI_pred_interval <- compute_interval(subIPI_df_features, subIPI_df_coeff, subIPI_pred, 3, 1, end_index = 286)

plot_interval(subIPI_pred, subIPI_pred_interval, 1)
plot_interval(subIPI_pred, subIPI_pred_interval, 2)
plot_interval(subIPI_pred, subIPI_pred_interval, 3)
plot_interval(subIPI_pred, subIPI_pred_interval, 4) + 
  xlab("Jahr") +
  ylab("Verbraucherpreisindex") +
  ggtitle("Verbraucherpreisindex (subIPI-Modell)",
          "rot = Wahrer Wert, blau = 1-Schritt-Vorhersage (95%-Interval)") +
  scale_x_continuous(breaks = seq(1, 56, 6),
                     labels = xlabel)


subUPI_pred_interval <- compute_interval(subUPI_df_features, subUPI_df_coeff, subUPI_pred, 1, 1, end_index = 286)

plot_interval(subUPI_pred, subUPI_pred_interval, 1)
plot_interval(subUPI_pred, subUPI_pred_interval, 2)
plot_interval(subUPI_pred, subUPI_pred_interval, 3)
plot_interval(subUPI_pred, subUPI_pred_interval, 4) 







merge_df <- function(pred, feat, i, lag){
  k <- ncol(feat)
  result_df <- as.data.frame(matrix(0,lag,k))
  if(lag == 1){
    result_df[1,] <- pred[i-1,]
    return(result_df)
  }
  
  result_df[lag,] <- pred[i - 1,]
  for(j in 2:lag){
    result_df[(lag+1)-j,] <- feat[i - j,]
  }
  
  return(result_df)
}

tmp = merge_df(subIPI_pred$Prediction ,subIPI_pred$`Wahrer Wert`, 4, 1)

RW_2step <- function(features, pred, coeff, start, end, lag){
  h <- start:end
  result <- as.data.frame(matrix(0, length(t), ncol(features)))
  error <- as.data.frame(matrix(0, length(t), ncol(features)))
  for(i in 1:length(start:end)){
    new_features <- merge_df(pred, features, h[i], lag)
    result[i,] <- prediction(rev(new_features), coeff) 
    error[i,] <- features[h[i],] - result[i,]
  }
  df <- list('Wahrer Wert' = features[h,], 'Prediction' = result,
             'Error' = error)
  
  return(df)
}


RW_2step(subIPI_pred$`Wahrer Wert`, subIPI_pred$Prediction, subIPI_df_coeff, 4, 56, 1)






Zins_irf1 <- irf(EPU_est, impulse = "EPU", response = "Zins",
            n.ahead = 10, ortho = TRUE, boot = FALSE)
Zins_irf2 <- irf(UPI_est, impulse = "UPI", response = "Zins",
            n.ahead = 10, ortho = TRUE, boot = FALSE)
Zins_irf3 <- irf(IPI_est, impulse = "IPI", response = "Zins",
            n.ahead = 10, ortho = TRUE, boot = FALSE)


plot(Zins_irf1$irf$EPU, type="l", ylim = c(-0.05, 0.13), xaxt = "n",
     ylab = "Geldmarktzins", xlab = "Zeitperiode",
     main = "EPU (Impuls) => Geldmarktzins (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))
plot(Zins_irf2$irf$UPI, type="l", ylim = c(-0.05, 0.13), xaxt = "n",
     ylab = "Geldmarktzins", xlab = "Zeitperiode",
     main = "UPI (Impuls) => Geldmarktzins (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))
plot(Zins_irf3$irf$IPI, type="l", ylim = c(-0.05, 0.13),  xaxt = "n",
     ylab = "Geldmarktzins", xlab = "Zeitperiode",
     main = "IPI (Impuls) => Geldmarktzins (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))



Arbeit_irf1 <- irf(EPU_est, impulse = "EPU", response = "Arbeit",
            n.ahead = 10, ortho = TRUE, boot = FALSE)
Arbeit_irf2 <- irf(UPI_est, impulse = "UPI", response = "Arbeit",
            n.ahead = 10, ortho = TRUE, boot = FALSE)
Arbeit_irf3 <- irf(IPI_est, impulse = "IPI", response = "Arbeit",
            n.ahead = 10, ortho = TRUE, boot = FALSE)

plot(Arbeit_irf1$irf$EPU, type="l", ylim = c(-0.02, 0.05), xaxt = "n",
     ylab = "Arbeitslosigkeit", xlab = "Zeitperiode",
     main = "EPU (Impuls) => Arbeitslosigkeit (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))
plot(Arbeit_irf2$irf$UPI, type="l", ylim = c(-0.02, 0.05), xaxt = "n",
     ylab = "Arbeitslosigkeit", xlab = "Zeitperiode",
     main = "UPI (Impuls) => Arbeitslosigkeit (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))
plot(Arbeit_irf3$irf$IPI, type="l", ylim = c(-0.02, 0.05), xaxt = "n",
     ylab = "Arbeitslosigkeit", xlab = "Zeitperiode",
     main = "IPI (Impuls) => Arbeitslosigkeit (Antwort)")
abline(h=0, col = "red")
axis(1, at = 1:11, labels = c(0:10))

#












'
# Testen auf Signifikanten Unterschied der Fehler
signif_error <- function(Wahr1, Pred1, Wahr2, Pred2){
  sqrt_error <- numeric(length(Wahr1))
  for (i in 1:length(Wahr1)) {
    error1 <- Wahr1[i]-Pred1[i]
    error2 <- Wahr2[i]-Pred2[i]
    
    sqrt_error[i] <- error2^2 - error1^2
  }
  error_mean <- mean(sqrt_error)
  sd_error <- sd(sqrt_error)
  
  t <- error_mean/(sd_error/sqrt(length(Wahr1)))
  p <- 2*pt(abs(t), length(Wahr1) - 1, lower.tail  = FALSE)
  return(c("p" = p, "t" =t))
}


signif_error(pred$`Wahrer Wert`$IP_difflog, pred$Prediction$V1, 
             pred_AR_IP$Wahrer.Wert, pred_AR_IP$Prediction)
signif_error(pred$`Wahrer Wert`$IP, pred$Prediction$V1, 
             IPI_pred$`Wahrer Wert`$IP, IPI_pred$Prediction$V1)
signif_error(pred$`Wahrer Wert`$IP, pred$Prediction$V1, 
             UPI_pred$`Wahrer Wert`$IP, UPI_pred$Prediction$V1)
signif_error(pred$`Wahrer Wert`$IP, pred$Prediction$V1, 
             EPU_pred$`Wahrer Wert`$IP, EPU_pred$Prediction$V1)
signif_error(pred$`Wahrer Wert`$IP, pred$Prediction$V1, 
             subIPI_pred$`Wahrer Wert`$IP, subIPI_pred$Prediction$V1)
signif_error(pred$`Wahrer Wert`$IP, pred$Prediction$V1, 
             subUPI_pred$`Wahrer Wert`$IP, subUPI_pred$Prediction$V1)


signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             pred_AR_Zins$Wahrer.Wert, pred_AR_Zins$Prediction)
signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             IPI_pred$`Wahrer Wert`$Zins, IPI_pred$Prediction$V2)
signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             UPI_pred$`Wahrer Wert`$Zins, UPI_pred$Prediction$V2)
signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             EPU_pred$`Wahrer Wert`$Zins, EPU_pred$Prediction$V2)
signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             subIPI_pred$`Wahrer Wert`$Zins, subIPI_pred$Prediction$V2)
signif_error(pred$`Wahrer Wert`$Zins, pred$Prediction$V2, 
             subUPI_pred$`Wahrer Wert`$Zins, subUPI_pred$Prediction$V2)


signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             pred_AR_Arbeit$Wahrer.Wert, pred_AR_Arbeit$Prediction)
signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             IPI_pred$`Wahrer Wert`$Arbeit, IPI_pred$Prediction$V3)
signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             UPI_pred$`Wahrer Wert`$Arbeit, UPI_pred$Prediction$V3)
signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             EPU_pred$`Wahrer Wert`$Arbeit, EPU_pred$Prediction$V3)
signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             subIPI_pred$`Wahrer Wert`$Arbeit, subIPI_pred$Prediction$V3)
signif_error(pred$`Wahrer Wert`$Arbeit, pred$Prediction$V3, 
             subUPI_pred$`Wahrer Wert`$Arbeit, subUPI_pred$Prediction$V3)


signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             pred_AR_VPI$Wahrer.Wert, pred_AR_VPI$Prediction)
signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             IPI_pred$`Wahrer Wert`$VPI, IPI_pred$Prediction$V4)
signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             UPI_pred$`Wahrer Wert`$VPI, UPI_pred$Prediction$V4)
signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             EPU_pred$`Wahrer Wert`$VPI, EPU_pred$Prediction$V4)
signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             subIPI_pred$`Wahrer Wert`$VPI, subIPI_pred$Prediction$V4)
signif_error(pred$`Wahrer Wert`$VPI, pred$Prediction$V4, 
             subUPI_pred$`Wahrer Wert`$VPI, subUPI_pred$Prediction$V4)

## 2-step

signif_error(pred_2step$`Wahrer Wert`$IP_difflog[-c(1:8)], pred_2step$Prediction$V1[-c(1:8)], 
             pred_AR_IP_2step$Wahrer.Wert, pred_AR_IP_2step$Prediction)
signif_error(pred_2step$`Wahrer Wert`$IP, pred_2step$Prediction$V1, 
             IPI_pred_2step$`Wahrer Wert`$IP_difflog, IPI_pred_2step$Prediction$V1)
signif_error(pred_2step$`Wahrer Wert`$IP, pred_2step$Prediction$V1, 
             UPI_pred_2step$`Wahrer Wert`$IP, UPI_pred_2step$Prediction$V1)
signif_error(pred_2step$`Wahrer Wert`$IP, pred_2step$Prediction$V1, 
             EPU_pred_2step$`Wahrer Wert`$IP, EPU_pred_2step$Prediction$V1)
signif_error(pred_2step$`Wahrer Wert`$IP, pred_2step$Prediction$V1, 
             subIPI_pred_2step$`Wahrer Wert`$IP, subIPI_pred_2step$Prediction$V1)
signif_error(pred_2step$`Wahrer Wert`$IP, pred_2step$Prediction$V1, 
             subUPI_pred_2step$`Wahrer Wert`$IP, subUPI_pred_2step$Prediction$V1)


signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             pred_AR_Zins_2step$Wahrer.Wert[-1], pred_AR_Zins_2step$Prediction[-1])
signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             IPI_pred_2step$`Wahrer Wert`$Zins, IPI_pred_2step$Prediction$V2)
signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             UPI_pred_2step$`Wahrer Wert`$Zins, UPI_pred_2step$Prediction$V2)
signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             EPU_pred_2step$`Wahrer Wert`$Zins, EPU_pred_2step$Prediction$V2)
signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             subIPI_pred_2step$`Wahrer Wert`$Zins, subIPI_pred_2step$Prediction$V2)
signif_error(pred_2step$`Wahrer Wert`$Zins, pred_2step$Prediction$V2, 
             subUPI_pred_2step$`Wahrer Wert`$Zins, subUPI_pred_2step$Prediction$V2)


signif_error(pred_2step$`Wahrer Wert`$Arbeit[-c(1:9)], pred_2step$Prediction$V3[-c(1:9)], 
             pred_AR_Arbeit_2step$Wahrer.Wert, pred_AR_Arbeit_2step$Prediction)
signif_error(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3, 
             IPI_pred_2step$`Wahrer Wert`$Arbeit, IPI_pred_2step$Prediction$V3)
signif_error(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3, 
             UPI_pred_2step$`Wahrer Wert`$Arbeit, UPI_pred_2step$Prediction$V3)
signif_error(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3, 
             EPU_pred_2step$`Wahrer Wert`$Arbeit, EPU_pred_2step$Prediction$V3)
signif_error(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3, 
             subIPI_pred_2step$`Wahrer Wert`$Arbeit, subIPI_pred_2step$Prediction$V3)
signif_error(pred_2step$`Wahrer Wert`$Arbeit, pred_2step$Prediction$V3, 
             subUPI_pred_2step$`Wahrer Wert`$Arbeit, subUPI_pred_2step$Prediction$V3)


signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             pred_AR_VPI_2step$Wahrer.Wert[-1], pred_AR_VPI_2step$Prediction[-1])
signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             IPI_pred_2step$`Wahrer Wert`$VPI, IPI_pred_2step$Prediction$V4)
signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             UPI_pred_2step$`Wahrer Wert`$VPI, UPI_pred_2step$Prediction$V4)
signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             EPU_pred_2step$`Wahrer Wert`$VPI, EPU_pred_2step$Prediction$V4)
signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             subIPI_pred_2step$`Wahrer Wert`$VPI, subIPI_pred_2step$Prediction$V4)
signif_error(pred_2step$`Wahrer Wert`$VPI, pred_2step$Prediction$V4, 
             subUPI_pred_2step$`Wahrer Wert`$VPI, subUPI_pred_2step$Prediction$V4)'
