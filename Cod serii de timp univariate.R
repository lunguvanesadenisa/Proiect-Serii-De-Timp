data <- Date_licenta
data
head(data)
names(data)          
str(data)

inflatie <- ts(data$`Rata inflaţiei(%)`, start = c(2004, 1), frequency = 4)
plot(inflatie, main = "Rata inflației în România (2004–2024)", ylab = "%", col = "darkgreen", lwd = 2)

# Împărțirea seriei în set de antrenare și testare
inflatie_train <- window(inflatie, end = c(2022, 4))
inflatie_test  <- window(inflatie, start = c(2023, 1))

# Verificăm lungimea
length(inflatie_train)  # ar trebui să fie 76 (19 ani × 4)
length(inflatie_test)   # ar trebui să fie 8 (2 ani × 4)

# Vizualizare comparativă
plot(inflatie, col = "black", lwd = 1.5, main = "Împărțire serie de timp: Training vs Test", ylab = "%")
lines(inflatie_train, col = "blue", lwd = 2)
lines(inflatie_test, col = "red", lwd = 2)
legend("topleft", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1, lwd = 2)

install.packages("tseries") 
library(tseries)
adf.test(inflatie_train)

inflatie_diff <- diff(inflatie_train)

# Grafic pentru vizualizare
plot(inflatie_diff, main = "Prima diferență a inflației", col = "purple", lwd = 2)

# Retestare ADF pe seria diferențiată
adf.test(inflatie_diff)

# Vizualizarea autocorelărilor pentru seria diferențiată - IDENTIFICARE MANUALA
par(mfrow = c(1, 2))  # grafic 1x2
acf(inflatie_diff, main = "ACF - inflație diferențiată")
pacf(inflatie_diff, main = "PACF - inflație diferențiată")
par(mfrow = c(1, 1))  # revenim la 1 grafic

install.packages("forecast")
library(forecast)

mod_auto <- auto.arima(inflatie_train)
mod_auto


# Estimăm modelul ARIMA(1,1,1)
mod_111 <- Arima(inflatie_train, order = c(1, 1, 1))

# Rezultatele modelului
summary(mod_111)

# Estimăm modelul SARIMA sugerat de auto.arima()
mod_sarima <- Arima(inflatie_train, order = c(2,0,0), seasonal = list(order = c(2,0,1), period = 4))

# Rezumat
summary(mod_sarima)


# Diagnostic reziduuri
checkresiduals(mod_sarima)
jarque.bera.test(residuals(mod_sarima))

install.packages("FinTS")
library(FinTS)
ArchTest(residuals(mod_sarima))



# Forecast pe următoarele 8 trimestre (2023–2024)
forecast_sarima <- forecast(mod_sarima, h = 8)

# Vizualizare forecast + interval de încredere
plot(forecast_sarima, main = "Prognoză inflație SARIMA (2023–2024)", ylab = "%")

# Valorile prognozate
forecast_sarima$mean

# Comparăm prognoza cu valorile reale (test set)
obs <- as.numeric(inflatie_test)
pred <- as.numeric(forecast_sarima$mean)

# Calculăm erori
rmse <- sqrt(mean((obs - pred)^2))
mae <- mean(abs(obs - pred))
mape <- mean(abs((obs - pred)/obs)) * 100  # doar dacă obs ≠ 0

# Afișare
rmse; mae; mape


#CERINTA 1 	Modele cu trend determinist sau stochastic


# Creează seria de timp
inflatie <- ts(data$`Rata inflaţiei(%)`, start = c(2004, 1), frequency = 4)

# Creează variabila timp
t <- 1:length(inflatie)

# Model de trend determinist
trend_inf <- lm(inflatie ~ t)
summary(trend_inf)

# Grafic cu linie de trend
plot(t, inflatie, type = "l", col = "blue", lwd = 2,
     main = "Rata inflației și trendul determinist", ylab = "%", xlab = "Trimestre")
lines(t, fitted(trend_inf), col = "red", lwd = 2)
legend("topleft", legend = c("Inflație", "Trend liniar"),
       col = c("blue", "red"), lty = 1, lwd = 2)


library(tseries)
adf.test(inflatie)



#CERINTA 2 STATIONARITATE

adf.test(inflatie)
library(urca)

# KPSS test pentru staționaritate în jurul mediei (null: staționar)
kpss_test <- ur.kpss(inflatie, type = "mu")
summary(kpss_test)

inflatie_diff <- diff(inflatie)

# Plotează seria diferențiată
plot(inflatie_diff, col = "darkgreen", main = "Inflație diferențiată", ylab = "Diferență %")
# ADF pe seria diferențiată
adf.test(inflatie_diff)

# KPSS pe seria diferențiată
kpss_diff <- ur.kpss(inflatie_diff, type = "mu")
summary(kpss_diff)

#graficul inflatiei originale 
plot(inflatie,
     main = "Evoluția ratei inflației în România (2004–2024)",
     ylab = "Rata inflației (%)",
     xlab = "Timp (trimestre)",
     col = "blue",
     lwd = 2)

# Adăugare grilă
grid()



# CERINTA 3 

# inflație întreagă
inflatie <- ts(data$`Rata inflaţiei(%)`, start = c(2004, 1), frequency = 4)

# Set train: 2004–2022, test: 2023–2024 (ultimii 8 trimestre)
train_length <- length(inflatie) - 8
inflatie_train <- window(inflatie, end = c(2022, 4))
inflatie_test <- window(inflatie, start = c(2023, 1))

library(forecast)
# Model Holt-Winters multiplicativ (sau aditiv, vezi nota mai jos)
mod_hw <- hw(inflatie_train, seasonal = "additive", h = 8)

# Grafic prognoză
plot(mod_hw, main = "Prognoză inflație – model Holt-Winters (2023–2024)",
     ylab = "%", xlab = "Timp", col = "darkred", lwd = 2)
lines(inflatie_test, col = "blue", lwd = 2)
legend("topleft", legend = c("Prognoză HW", "Valori reale (test)"),
       col = c("darkred", "blue"), lty = 1, lwd = 2)

# Extragem valori
pred_hw <- as.numeric(mod_hw$mean)
obs_hw <- as.numeric(inflatie_test)

# Erori
rmse_hw <- sqrt(mean((obs_hw - pred_hw)^2))
mae_hw <- mean(abs(obs_hw - pred_hw))
mape_hw <- mean(abs((obs_hw - pred_hw)/obs_hw)) * 100

# Afișare
rmse_hw; mae_hw; mape_hw


# CERINTA 4 

# Împărțirea seriei
inflatie_train <- window(inflatie, end = c(2022, 4))
inflatie_test  <- window(inflatie, start = c(2023, 1))
library(forecast)

# Model SARIMA selectat automat
mod_sarima <- auto.arima(inflatie_train, seasonal = TRUE)
summary(mod_sarima)
# Prognoza pe 8 trimestre (2 ani)
forecast_sarima <- forecast(mod_sarima, h = 8)

# Vizualizare grafic
plot(forecast_sarima, main = "Prognoză inflație – model SARIMA", ylab = "%", xlab = "Timp")
lines(inflatie_test, col = "blue", lwd = 2)
legend("topleft", legend = c("Prognoză SARIMA", "Valori reale"), col = c("black", "blue"), lty = 1, lwd = 2)

# Extragem valorile reale și prognozate
obs <- as.numeric(inflatie_test)
pred <- as.numeric(forecast_sarima$mean)

# Calculul erorilor
rmse_sarima <- sqrt(mean((obs - pred)^2))
mae_sarima <- mean(abs(obs - pred))
mape_sarima <- mean(abs((obs - pred)/obs)) * 100

# Afișare
rmse_sarima; mae_sarima; mape_sarima
# Reziduuri + test Ljung-Box (autocorelație)
checkresiduals(mod_sarima)

# Test normalitate
library(tseries)
jarque.bera.test(residuals(mod_sarima))

# Test ARCH (heteroscedasticitate)
library(FinTS)
ArchTest(residuals(mod_sarima))

install.packages(c("readxl", "forecast", "ggplot2", "tseries"))
library(readxl)
library(forecast)
library(ggplot2)
library(tseries)

#Cerinta 5
Date_licenta <- read_excel("C:/Users/user1/Desktop/Date_licenta.xlsx")
View(Date_licenta)
inflatie <- as.numeric(Date_licenta$`Rata inflaţiei(%)`)

#Transformare în serie de timp trimestrială
inflatie_ts <- ts(inflatie, start = c(2004, 1), frequency = 4)

#Delimitare seturi training și test
train <- window(inflatie_ts, end = c(2019, 4))  # până la final de 2019
test <- window(inflatie_ts, start = c(2020, 1)) # de la începutul lui 2020
h <- length(test)  # orizontul de prognoză

#Model ARIMA 
install.packages("forecast")
library(forecast)

model_arima <- auto.arima(train)

#Predicție punctuală și pe interval
forecast_arima <- forecast(model_arima, h = h, level = 95)

#Afișare rezultate
print(forecast_arima)

#Transformare în serie de timp trimestrială (2004–2024)
inflatie_ts <- ts(inflatie, start = c(2004, 1), frequency = 4)

# 3. Delimitare seturi training și test
# Training: 2004–2022 (T4)
# Test: 2023–2024 (T1–T4)
train <- window(inflatie_ts, end = c(2022, 4))
test <- window(inflatie_ts, start = c(2023, 1))
h <- length(test)  # Orizont de prognoză: 8 trimestre

# Construire model ARIMA  pe setul de training
model_arima <- auto.arima(train)

#Prognoză pe perioada de test (2023–2024)
forecast_arima <- forecast(model_arima, h = h, level = 95)

#Afișare predicții: punctuale și pe interval de încredere
print(forecast_arima)
# Plot: predicția + intervalul de încredere + comparație cu test
autoplot(forecast_arima) +
  autolayer(test, series = "Valori reale (test)", color = "red") +
  ggtitle("Predicția ARIMA – rata inflației (2023–2024)") +
  xlab("Timp") + ylab("Rata inflației (%)") +
  theme_minimal() +
  guides(colour = guide_legend(title = "Serie"))

#Cerinta 6

# Pachete necesare
install.packages(c("readxl", "forecast", "Metrics"))
library(readxl)
library(forecast)
install.packages("Metrics")
library(Metrics)

# Încărcare date și creare serie de timp
data <- read_excel("Date licenta.xlsx", sheet = "Sheet2")
inflatie <- as.numeric(data$`Rata inflaţiei(%)`)
inflatie_ts <- ts(inflatie, start = c(2004, 1), frequency = 4)

#Seturi: training (2004–2022), test (2023–2024)
train <- window(inflatie_ts, end = c(2022, 4))
test <- window(inflatie_ts, start = c(2023, 1))
h <- length(test)

# Model Holt-Winters aditiv
model_hw <- HoltWinters(train)
forecast_hw <- forecast(model_hw, h = h)

#Model SARIMA (auto.arima detectează sezonalitatea)
model_sarima <- auto.arima(train, seasonal = FALSE)
forecast_sarima <- forecast(model_sarima, h = h)

# Acuratețe: MAE și RMSE
mae_hw <- mae(test, forecast_hw$mean)
rmse_hw <- rmse(test, forecast_hw$mean)

mae_sarima <- mae(test, forecast_sarima$mean)
rmse_sarima <- rmse(test, forecast_sarima$mean)

# Afișare rezultate
cat("Holt-Winters:\n  MAE =", mae_hw, "\n  RMSE =", rmse_hw, "\n\n")
cat("SARIMA:\n  MAE =", mae_sarima, "\n  RMSE =", rmse_sarima, "\n")

#Creare data frame cu toate seriile
timpuri <- time(test)

df_plot <- data.frame(
  Timp = timpuri,
  Real = as.numeric(test),
  SARIMA = as.numeric(forecast_sarima$mean),
  HW = as.numeric(forecast_hw$mean)
)

# Plot comparativâ
library(ggplot2)
ggplot(df_plot, aes(x = Timp)) +
  geom_line(aes(y = Real, color = "Valori reale"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = SARIMA, color = "SARIMA"), size = 1, linetype = "dashed") +
  geom_line(aes(y = HW, color = "Holt-Winters"), size = 1, linetype = "dotdash") +
  scale_color_manual(values = c("Valori reale" = "red", "SARIMA" = "blue", "Holt-Winters" = "darkgreen")) +
  ggtitle("Compararea modelelor SARIMA și Holt-Winters – rata inflației (2023–2024)") +
  xlab("Timp (trimestre)") +
  ylab("Rata inflației (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())

