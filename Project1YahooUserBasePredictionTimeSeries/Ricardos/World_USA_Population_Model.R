#I. Read different dataframes
#a. World population
world.population.historical <- read.csv(file.choose(), header=TRUE, sep=";")
#b. US population
usa.population.historical <- read.csv(file.choose(), header=TRUE, sep=";")

#------------------------------------------------------------------------------

#II. Read evolution of active users and revenue data (facebook only)
#a. worldwide
#world.facebook.historical <- read.csv(file.choose(), header=TRUE, sep=";")
#b. USA
#usa.facebook.historical <- read.csv(file.choose(), header=TRUE, sep=";")

#------------------------------------------------------------------------------

#III. Define and plot decomposed time series
#a. World population
world.population.ts <- ts(world.population.historical$People,start=c(2010,4), frequency=12)
plot(world.population.ts)
#a1. Additive
world.population.fit.additive <- decompose(world.population.ts , type="additive")
plot(world.population.fit.additive)
#a2. Multiplicative
world.population.fit.multiplicative <- decompose(world.population.ts , type="multiplicative")
plot(world.population.fit.multiplicative)
#a3. STL
world.population.fit.stl <- stl(world.population.ts, t.window=12, s.window="periodic", robust=TRUE)
plot(world.population.fit.stl)

#b. US population
usa.population.ts <- ts(usa.population.historical$People,start=c(2010,4), frequency=12)
plot(usa.population.ts)
#a1. Additive
usa.population.fit.additive <- decompose(usa.population.ts , type="additive")
plot(usa.population.fit.additive)
#a2. Multiplicative
usa.population.fit.multiplicative <- decompose(usa.population.ts , type="multiplicative")
plot(usa.population.fit.multiplicative)
#a3. STL
usa.population.fit.stl <- stl(usa.population.ts, t.window=12, s.window="periodic", robust=TRUE)
plot(usa.population.fit.stl)

#------------------------------------------------------------------------------

#IV. Create exponential smoothing models
library("forecast")

#a. World population

#a1. Create exponential models
world.population_AAN <- ets(world.population.ts, model="AAN", damped=TRUE)
world.population_AAZ <- ets(world.population.ts, model="AAZ", damped=TRUE)
world.population_MMN <- ets(world.population.ts, model="MMN", damped=TRUE)
world.population_MMZ <- ets(world.population.ts, model="MMZ", damped=TRUE)
#world.population_ZZZ <- ets(world.population.ts, model="ZZZ", damped=TRUE)

#plot(world.population_AAN)
#plot(world.population_AAZ)
#plot(world.population_MMN)
#plot(world.population_MMZ)
#plot(world.population_ZZZ)

#a2. Create prediction "cones"
world.population_AAN_pred <- forecast(world.population_AAN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
world.population_AAZ_pred <- forecast(world.population_AAZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))
world.population_MMN_pred <- forecast(world.population_MMN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
world.population_MMZ_pred <- forecast(world.population_MMZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))
#world.population_ZZZ_pred <- forecast(world.population_ZZZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))

#a3. Plot prediction "cones"
par(mfrow=c(1,1))
plot(world.population_AAN_pred, xlab="Year", ylab="Predicted World Population")
plot(world.population_MMN_pred, xlab="Year", ylab="Predicted World Population")
plot(world.population_MMZ_pred, xlab="Year", ylab="Predicted World Population")
#plot(world.population_ZZZ_pred, xlab="Year", ylab="Predicted World Population")

#b. USA population
#b1. Create exponential models
usa.population_AAN <- ets(usa.population.ts, model="AAN", damped=TRUE)
usa.population_AAZ <- ets(usa.population.ts, model="AAZ", damped=TRUE)
usa.population_MMN <- ets(usa.population.ts, model="MMN", damped=TRUE)
usa.population_MMZ <- ets(usa.population.ts, model="MMZ", damped=TRUE)
#usa.population_ZZZ <- ets(usa.population.ts, model="ZZZ", damped=TRUE)

#plot(usa.population_AAN)
#plot(usa.population_AAZ)
#plot(usa.population_MMN)
#plot(usa.population_MMZ)
#plot(usa.population_ZZZ)

#b2. Create prediction "cones"
usa.population_AAN_pred <- forecast(usa.population_AAN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
usa.population_AAZ_pred <- forecast(usa.population_AAZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))
usa.population_MMN_pred <- forecast(usa.population_MMN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
usa.population_MMZ_pred <- forecast(usa.population_MMZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))
#usa.population_ZZZ_pred <- forecast(usa.population_ZZZ, h=115, level=c(0.2, 0.4, 0.6, 0.8))

#b3. Plot prediction "cones"
par(mfrow=c(1,3))
plot(usa.population_MMZ_pred, xlab="Year", ylab="Predicted USA Population")
plot(usa.population_AAZ_pred, xlab="Year", ylab="Predicted USA Population")
#plot(usa.population_ZZZ_pred, xlab="Year", ylab="Predicted USA Population")

#------------------------------------------------------------------------------

#V. Create TBATS models

#a. World population
#a1. Create TBATS models
world.population_tbats <- tbats(world.population.ts)

#a2. Create prediction "cones"
world.population_tbats_pred <-forecast(world.population_tbats, h=115)

#a3. Plot prediction "cones" (TBATS and exponential)
par(mfrow=c(1,1))
plot(world.population_AAZ_pred, xlab="Year", ylab="Predicted World Population")
plot(world.population_MMZ_pred, xlab="Year", ylab="Predicted World Population")
plot(world.population_tbats_pred, xlab="Year", ylab="Predicted World Population")

#b. USA population
#b1. Create TBATS models
usa.population_tbats <- tbats(usa.population.ts)

#b2. Create prediction "cones"
usa.population_tbats_pred <-forecast(usa.population_tbats, h=115)

#b3. Plot prediction "cones" (TBATS and exponential)
par(mfrow=c(1,1))
plot(usa.population_AAZ_pred, xlab="Year", ylab="Predicted USA Population")
plot(usa.population_MMZ_pred, xlab="Year", ylab="Predicted USA Population")
plot(usa.population_tbats_pred, xlab="Year", ylab="Predicted USA Population")

#------------------------------------------------------------------------------

#VI. Export mean predictions for the different models (exponential and TBATS models)

#a. World population
#a1. Look to the models
world.population_AAZ
world.population_MMZ
world.population_ZZZ
world.population_tbats

#a2. Print the mean predictions from the seasonal models 
cbind(world.population_AAZ_pred$mean, world.population_MMZ_pred$mean,world.population_ZZZ_pred$mean, world.population_tbats_pred$mean)

#a3. Extract mean predictions to .csv file 
write.csv(world.population_AAZ_pred$mean, file = "Predicted World Population_AAZ.csv")
write.csv(world.population_MMZ_pred$mean, file = "Predicted World Population_MMZ.csv")
write.csv(world.population_ZZZ_pred$mean, file = "Predicted World Population_ZZZ.csv")
write.csv(world.population_tbats_pred$mean, file = "Predicted World Population_TBATS.csv")

#b. USA population
#a1. Look to the models
usa.population_AAZ
usa.population_MMZ
usa.population_ZZZ
usa.population_tbats

#b2. Print the mean predictions from the seasonal models 
cbind(usa.population_AAZ_pred$mean, usa.population_MMZ_pred$mean, usa.population_ZZZ_pred$mean, usa.population_tbats_pred$mean)

#b3. Extract mean predictions to .csv file 
write.csv(usa.population_AAZ_pred$mean, file = "Predicted USA Population_AAZ.csv")
write.csv(usa.population_MMZ_pred$mean, file = "Predicted USA Population_MMZ.csv")
write.csv(usa.population_ZZZ_pred$mean, file = "Predicted USA Population_ZZZ.csv")
write.csv(usa.population_tbats_pred$mean, file = "Predicted USA Population_TBATS.csv")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#VII. Create ARIMA model
library("fpp")

#a. World population
#a1. Identify auto-correlation function
par(mfrow=c(1,1))
Acf(world.population.ts,main="")

#a2. Create ARIMA model
world.population.ARIMA.fit <- auto.arima(world.population.ts,seasonal=TRUE)
world.population.ARIMA.fit

Acf(residuals(world.population.ARIMA.fit))
plot(forecast(world.population.ARIMA.fit,115)) 

#a3. Print the mean predictions from the ARIMA model 
world.population.ARIMA_pred <- forecast(world.population.ARIMA.fit,115)
write.csv(world.population.ARIMA_pred$mean, file = "Predicted World Population_ARIMA.csv")

#B. USA population
#b1. Identify auto-correlation function
par(mfrow=c(1,1))
Acf(usa.population.ts,main="")

#b2. Create ARIMA model
usa.population.ARIMA.fit <- auto.arima(usa.population.ts,seasonal=TRUE)
usa.population.ARIMA.fit

Acf(residuals(usa.population.ARIMA.fit))
plot(forecast(usa.population.ARIMA.fit,115)) 

#b3. Print the mean predictions from the ARIMA model 
usa.population.ARIMA_pred <- forecast(usa.population.ARIMA.fit,115)
write.csv(usa.population.ARIMA_pred$mean, file = "Predicted USA Population_ARIMA.csv")

