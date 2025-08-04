install.packages(c("readxl", "urca", "vars", "tseries", "lmtest", "forecast"))
library(readxl)
library(urca)
library(vars)
library(tseries)
library(lmtest)
library(forecast)

date <- read_excel("Date licenta.xlsx")

ts_pib <- ts(date$`PIB (milioane lei)`, start = c(2004, 1), frequency = 4)
ts_var_pib <- ts(date$`Variația procentuală a PIB-ului faţă de perioada corespunzătoare din anul precedent`, start = c(2004, 1), frequency = 4)
ts_ipc <- ts(date$`IPC(%)`, start = c(2004, 1), frequency = 4)
ts_inflatie <- ts(date$`Rata inflaţiei(%)`, start = c(2004, 1), frequency = 4)
ts_dobanda <- ts(date$`Rata dobânzii(%)`, start = c(2004, 1), frequency = 4)
ts_curs <- ts(date$`Cursul valutar(EURO în LEI)`, start = c(2004, 1), frequency = 4)
ts_somaj <- ts(date$`Rata şomajului`, start = c(2004, 1), frequency = 4)

#Test ADF pentru staționaritate
test_stationaritate <- function(serie, nume) {
  adf <- ur.df(serie, type = "trend", lags = 4)
  kpss <- ur.kpss(serie, type = "tau")
  cat("=== Rezultate pentru", nume, "===\n")
  cat("ADF p-value:", adf@testreg$coefficients[4, 4], "\n")
  cat("KPSS statistic:", kpss@teststat, "\n\n")
}

test_stationaritate(ts_pib, "PIB")
ndiffs(ts_pib)
ndiffs(ts_var_pib)
ndiffs(ts_inflatie)
ndiffs(ts_dobanda)
ndiffs(ts_curs)
ndiffs(ts_somaj)
test_stationaritate(ts_var_pib, "Variația PIB")
test_stationaritate(ts_inflatie, "Inflație")
test_stationaritate(ts_dobanda, "Dobândă")
test_stationaritate(ts_curs, "Curs EUR")
test_stationaritate(ts_somaj, "Șomaj")
d_log_pib <- ts(diff(log(ts_pib)), start = c(2004, 2), frequency = 4)
d_log_dobanda <- ts(diff(log(ts_dobanda)), start = c(2004, 2), frequency = 4)
d_ts_curs <- ts(diff(ts_curs), start = c(2004, 2), frequency = 4)
d_ts_somaj <- ts(diff(ts_somaj), start = c(2004, 2), frequency = 4)

#Test de cointegrare Johansen
johansen_test <- ca.jo(cbind(ts_curs, ts_somaj), type = "trace", K = 2)
summary(johansen_test)

#Alegerea lagului optim
date_finale <- ts.union(
  d_log_pib,
  ts_var_pib = window(ts_var_pib, start = c(2004, 2)),
  ts_inflatie = window(ts_inflatie, start = c(2004, 2)),
  d_log_dobanda,
  d_ts_curs,
  d_ts_somaj
)

#Selectare lag optim
lagselect <- VARselect(date_finale,lag.max = 8, type = 'const')
lagselect
lagselect$selection
lag_optim <- lagselect$selection["FPE(n)"]

# Estimare VAR
var_model <- VAR(date_finale, p = lag_optim, type = "const")
summary(var_model)

#Teste Granger
variabile_interes <- setdiff(colnames(date_finale_clean), c("d_log_pib", "ts_var_pib"))

for (var in variabile_interes) {
  test <- grangertest(d_log_pib ~ get(var), 
                      data = as.data.frame(date_finale_clean), 
                      order = lag_optim)
  cat(var, "-> d_log_pib : p-value =", round(test$`Pr(>F)`[2], 4), 
      ifelse(test$`Pr(>F)`[2] < 0.05, "*CAUZAL*", ""), "\n")
}
#IRF
irf_curs_pib <- irf(var_model, impulse = "d_ts_curs", response = "d_log_pib", n.ahead = 10)
plot(irf_curs_pib, main = "Răspunsul PIB-ului la un șoc în cursul valutar")

#Descompunerea varianței
fevd_results <- fevd(var_model, n.ahead = 10)
print(fevd_results$d_log_pib)
fevd_plot <- fevd(var_model, n.ahead = 10)
plot(fevd_plot, main = "Contribuția variabilelor la varianța PIB-ului")
