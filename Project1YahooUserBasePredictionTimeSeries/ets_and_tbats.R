#install.packages("forecast") #-- do this only once 
#Check the book: https://www.otexts.org/fpp and the blog: http://robjhyndman.com/hyndsight/forecasting/ 
library("forecast")

Yahoo_input_data<-read.csv(file.choose(), header=TRUE, sep=",")
Yahoo_US_ts <- ts(Yahoo_input_data$PeopleUS,start=2010, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 
Yahoo_W_ts <- ts(Yahoo_input_data$PeopleW,start=2010, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 



#plot various decompositions into error/noise, trend and seasonality

fit <- decompose(Yahoo_US_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(Yahoo_US_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fit <- stl(Yahoo_US_ts, t.window=12, s.window="periodic", robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit)

plot(Yahoo_US_ts)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
Yahoo_US_AAZ <- ets(Yahoo_US_ts, model="AAZ", damped=TRUE)
Yahoo_US_AAZ # Lets look at what our models actually are
Yahoo_US_MMZ <- ets(Yahoo_US_ts, model="MMZ", damped=TRUE)
Yahoo_US_MMZ # Lets look at what our models actually are
Yahoo_W_AAZ <- ets(Yahoo_W_ts, model="AAZ", damped=TRUE)
Yahoo_W_AAZ # Lets look at what our models actually are
Yahoo_W_MMZ <- ets(Yahoo_W_ts, model="MMZ", damped=TRUE)
Yahoo_W_MMZ # Lets look at what our models actually are

# Create their prediction "cones" for 120 months (10 years) into the future with quintile confidence intervals
Yahoo_US_AAZ_pred <- forecast(Yahoo_US_AAZ, h=120, level=c(0.2, 0.4, 0.6, 0.8))
Yahoo_US_MMZ_pred <- forecast(Yahoo_US_MMZ, h=120, level=c(0.2, 0.4, 0.6, 0.8))
Yahoo_W_AAZ_pred <- forecast(Yahoo_W_AAZ, h=120, level=c(0.2, 0.4, 0.6, 0.8))
Yahoo_W_MMZ_pred <- forecast(Yahoo_W_MMZ, h=120, level=c(0.2, 0.4, 0.6, 0.8))

#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
Yahoo_US_tbats <- tbats(Yahoo_US_ts)
Yahoo_US_tbats_pred <-forecast(Yahoo_US_tbats, h=120)
Yahoo_US_tbats # Lets look at what our models actually are
Yahoo_W_tbats <- tbats(Yahoo_W_ts)
Yahoo_W_tbats_pred <-forecast(Yahoo_W_tbats, h=120)
Yahoo_W_tbats # Lets look at what our models actually are

#ARIMA model could also be fitted but does not work well in this case. Need ARIMA with covariates (e.g., Fourier)
Yahoo_US_arima <- auto.arima(Yahoo_US_ts, seasonal=TRUE)
Yahoo_US_arima
Yahoo_US_arima_pred <-forecast(Yahoo_US_arima, h=120)
Yahoo_W_arima <- auto.arima(Yahoo_W_ts, seasonal=TRUE)
Yahoo_W_arima
Yahoo_W_arima_pred <-forecast(Yahoo_W_arima, h=120)

#Plot forecasts
par(mfrow=c(2,2)) # Lets look at the three models with seasonality on one graph on the same scale
plot(Yahoo_US_AAZ_pred, xlab="Year", ylab="Predicted MAU") #, ylim=c(0,0.3))
plot(Yahoo_US_MMZ_pred, xlab="Year", ylab="Predicted MAU") #, ylim=c(0,0.3))
plot(Yahoo_US_tbats_pred, xlab="Year", ylab="Predicted MAU")# , ylim=c(0,0.3))
plot(Yahoo_US_arima_pred, ylab="Predicted MAU")
plot(Yahoo_W_AAZ_pred, xlab="Year", ylab="Predicted MAU") #, ylim=c(0,0.3))
plot(Yahoo_W_MMZ_pred, xlab="Year", ylab="Predicted MAU") #, ylim=c(0,0.3))
plot(Yahoo_W_tbats_pred, xlab="Year", ylab="Predicted MAU")# , ylim=c(0,0.3))
plot(Yahoo_W_arima_pred, ylab="Predicted MAU")


# Print the mean predictions from the three seasonal models to the console screen to copy and paste from the screen to Excel 
models <- cbind(Yahoo_US_AAZ_pred$mean, Yahoo_US_MMZ_pred$mean, Yahoo_US_tbats_pred$mean, Yahoo_US_arima_pred$mean,Yahoo_W_AAZ_pred$mean, Yahoo_W_MMZ_pred$mean, Yahoo_W_tbats_pred$mean, Yahoo_W_arima_pred$mean)
write.csv(models, file = "Predicted_MAU_all_models.csv")

#print details of each of the models to csv (e.g. the quantiles)
write.csv(Yahoo_US_AAZ_pred$mean, file = "Predicted_MAU_US_AAZ.csv") # export the selected model's predictions into a CSV file
write.csv(Yahoo_US_MMZ_pred$mean, file = "Predicted_MAU_US_MMZ.csv") # export the selected model's predictions into a CSV file
write.csv(Yahoo_US_tbats_pred$mean, file = "Predicted_MAU_US_tbats.csv") # export the selected model's predictions into a CSV file
write.csv(Yahoo_US_tbats_pred$mean, file = "Predicted_MAU_US_tbats.csv") # export the selected model's predictions into a CSV file



