library(ggfortify)

set.seed(123)
ar1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = 200)
autoplot(ar1, main = "AR1")
autoplot(acf(ar1, plot = F), main = "AR1 - ACF")
autoplot(pacf(ar1, plot = F), main = "AR1 - PACF")

set.seed(123)
ma1 <- arima.sim(list(order = c(0, 0, 1), ma = -0.5), n = 200)
autoplot(ma1, main = "MA1")
autoplot(acf(ma1, plot = F), main = "MA1 - ACF")
autoplot(pacf(ma1, plot = F), main = "MA1 - PACF")

climate <- read.csv("climate.csv", stringsAsFactors = F)
str(climate)

climate <- ts(climate[, 2:3], frequency = 1, 
              start = 1919, end = 2013)
head(climate)

library(forecast)
library(tseries)

plot(climate)


cor(climate)

autoplot(acf(climate[, 2], plot = F), main="Temp ACF")
autoplot(pacf(climate[, 2], plot = F), main="Temp PACF")
autoplot(acf(climate[, 1], plot = F), main="CO2 ACF")
autoplot(pacf(climate[, 1], plot = F), main = "CO2 PACF")

ccf(climate[, 1], climate[, 2], main = "CCF") 

adf.test(climate[, 1])
adf.test(climate[, 2])

temp <- climate[, 2]
train <- window(temp, start = 1946, end = 2003)

test <- window(temp, start = 2004)

fit.holt <- holt(train, h = 10, initial = "optimal")
# summary(fit.holt)
plot(forecast(fit.holt))
lines(test, type= "o")

fit.holtd <- holt(train, h = 10, initial = "optimal", damped = TRUE)
# summary(fit.holtd)
plot(forecast(fit.holtd), main ="Holt Damped")
lines(test, type = "o")

fit.arima <- auto.arima(train)
summary(fit.arima)
plot(forecast(fit.arima, h = 10))
lines(test, type="o")

mapeHOLT <- sum(abs((test - fit.holt$mean)/test))/10
mapeHOLT
mapeHOLTD <- sum(abs((test - fit.holtd$mean)/test))/10
mapeHOLTD
mapeARIMA <- sum(abs((test - forecast(fit.arima, h = 10)$mean)/test))/10
mapeARIMA

fit.lm <- lm(Temp ~ CO2, data = climate)
summary(fit.lm)
plot.ts(fit.lm$residuals)
acf(fit.lm$residuals)
dwtest(fit.lm)

######granger causality

ndiffs(climate[, 1], test = "adf")
ndiffs(climate[, 2], test = "adf")

library(vars)
library(aod)

climateDiff <- diff(climate)
climateDiff <- window(climateDiff, start = 1946)
head(climateDiff)

lag.select <- VARselect(climateDiff, lag.max = 12)
lag.select$selection

fit1 <- VAR(climateDiff, p = 5)
summary(fit1)
serial.test(fit1, type = "PT.asymptotic")

x2y <- causality(fit1, cause = "CO2")
y2x <- causality(fit1, cause = "Temp")
x2y$Granger
y2x$Granger

climateLevels <- window(climate, start = 1946)
level.select <- VARselect(climateLevels, lag.max = 12)
level.select$selection

fit2 <- VAR(climateLevels, p = 7)

serial.test(fit2, type = "BG")

CO2terms <- seq(1, 11, 2)
Tempterms <- seq(2, 12, 2)

wald.test(b = coef(fit2$varresult$Temp),
          Sigma = vcov(fit2$varresult$Temp),
          Terms = c(CO2terms))


wald.test(b = coef(fit2$varresult$CO2),
          Sigma = vcov(fit2$varresult$CO2),
          Terms = c(Tempterms))


autoplot(predict(fit2, n.ahead=25, ci=0.95))

