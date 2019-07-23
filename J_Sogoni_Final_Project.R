setwd("C:/Users/JSogoni/Google Drive/Rutgers/Big Data & Economic Forecasting/Final Project")
data <- read.csv("Oil.csv")
library(fpp)

#Q1
hist (data$WTI, breaks=20, freq=FALSE, col="blue")
lines (density(data$WTI) , col="red", lwd=2)

#Q2
WTI <- ts(data$WTI,frequency=12, start=c(1987,5))
seasonplot(WTI, year.labels=TRUE, col="blue")
monthplot(WTI, col.base="red")

#Q3
library(urca)
t0 = c(1987,5)
t1 = c(2004,12)
t2 = c(2005,1)
WTI_train <- window(WTI, start=t0, end=t1)
WTI_test <- window(WTI, start=t2)
WTI_ADF_none <- ur.df(WTI_train, type="none", lags=12, selectlags = "AIC")
summary(WTI_ADF_none)
WTI_ADF_drift <- ur.df(WTI_train, type="drift", lags=12, selectlags = "AIC")
summary(WTI_ADF_drift)
WTI_ADF_trend <- ur.df(WTI_train, type="trend", lags=12, selectlags = "AIC")
summary(WTI_ADF_trend)
#
DWTI <-diff(WTI_train)
DWTI_ADF_none <- ur.df(DWTI, type="none", lags=12, selectlags = "AIC")
summary(DWTI_ADF_none)
DWTI_ADF_drift <- ur.df(DWTI, type="drift", lags=12, selectlags = "AIC")
summary(DWTI_ADF_drift)
DWTI_ADF_trend <- ur.df(DWTI, type="trend", lags=12, selectlags = "AIC")
summary(DWTI_ADF_trend)



#Q4
auto.arima(DWTI, max.p=5, max.q=5,ic="aic", max.order=10, seasonal=FALSE, stepwise=FALSE)
auto.arima(DWTI, max.p=5, max.q=5,ic="bic", max.order=10, seasonal=FALSE, stepwise=FALSE)

#Q5
library(vars)
Brent_train <- data$Brent[0:212]
Brent_train
Brent_test <- data$Brent[213:224]
Brent_test
df_level <- data.frame(WTI_train, Brent_train)
df_level
vecm <- ca.jo(df_level, type="eigen", ecdet="const", K=2, spec="transitory")
summary(vecm)
vecm.rls <- cajorls(vecm, r = 1)
error <- vecm.rls$rlm$model["ect1"]
error.ADF <- ur.df(error$ect1, type="none",lags=12, selectlags = "AIC")
summary(error.ADF)
plot(error$ect1)
acf(error$ect1)
pacf(error$ect1)
cajorls(vecm)
coef(summary(cajorls(vecm)$rlm))

#Q6
var.model = vec2var(vecm)
H = 12
fc <- predict(var.model, n.ahead=H)
WTI_forecast <- ts(fc$fcst$WTI_train[1:H,1], frequency=12, start=t2)
Brent_forecast <- ts(fc$fcst$Brent_train[1:H,1], frequency=12, start=t2)
accuracy(WTI_forecast,WTI_test)
accuracy(Brent_forecast,Brent_test)
