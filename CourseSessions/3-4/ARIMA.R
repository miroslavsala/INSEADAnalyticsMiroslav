#install.packages("forecast","fpp") #-- do this only once 
#These packages are from the "Forecasting Principles and Practice" - excellent and free book: https://www.otexts.org/fpp
library("forecast")
library("fpp")


# DIFFERENCING and ARIMA

# we will use the US consumption and income dataset (comes with fpp "book")
plot(usconsumption)
View(usconsumption)

par(mfrow=c(1,2))

plot(usconsumption[,1], xlab="Year", ylab="US consumption") #plotting consumption (by quarter)
plot(diff(usconsumption[,1],1), xlab="Year", ylab="change in US consumption") # differencing: plotting change in consumptoin from one quarter to the next

par(mfrow=c(1,1))
Acf(usconsumption[,1],main="") # ACF = auto-correlation function

fit <- auto.arima(usconsumption[,1],seasonal=FALSE) #automatically fits the ARIMA model (auto-regressive integrated moving average)
fit

Acf(residuals(fit))
plot(forecast(fit,20)) #20 stands for 20 quarters = 5 years

# SEASONAL ARIMA

# we will use a10 dataset (also comes with fpp - sales of antidiabetic drug)
par(mfrow=c(1,3))
View(a10)
plot(a10, xlab="Year",
     ylab="A10 sales")
plot(log(a10), xlab="Year",
     ylab="log A10 sales")
plot(diff(log(a10),12), xlab="Year",
     ylab="Annual change in monthly log A10 sales")

fit <- stl(log(a10), t.window=12, s.window="periodic", robust=TRUE)
plot(fit)

fit <- auto.arima(a10,seasonal=FALSE)
fit

fit <- auto.arima(a10,seasonal=TRUE)
fit

par(mfrow=c(1,1))
Acf(residuals(fit))
plot(forecast(fit,60)) #60 stands for 60 months = 5 years

# ARIMA with regressors ("dynamic regression")

# with insurance and advertising data (also part of FPP)
plot(insurance, main="Insurance advertising and quotations", xlab="Year")
View(insurance)

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")
Advert


# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

AIC(fit1) #criterium to find quality of the model - the lower is better
AIC(fit2)
AIC(fit3)
AIC(fit4)

#Best fit (as per AIC) is with all data (1:2), so the final model becomes
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fit

# forecast insurance quotes with advertising = 10
fc10 <- forecast(fit, xreg=cbind(rep(10,20),c(Advert[40,1],rep(10,19))), h=20)
plot(fc10, main="Forecast quotes with advertising set to 10", ylab="Quotes")

# see how forecasts with advertising = 8 will differ from advertising = 2
par(mfrow=c(1,2))
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")

fc2 <- forecast(fit, xreg=cbind(rep(2,20),c(Advert[40,1],rep(2,19))), h=20)
plot(fc2, main="Forecast quotes with advertising set to 2", ylab="Quotes")

