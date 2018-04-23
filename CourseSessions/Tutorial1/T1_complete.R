# You are selling stuffed toy animals for holiday season. 
# Each animal first is sold in a test market, and the "test" data is recorded. 
# Then the selected toys are sold in all markets, and the total "sales" data is recorded. 
# In addition, the kind of the toy is recorded, "SKU". 
# The goal is to predict the market sales of a "Bear" for which the test sales = 10 items.

# I. Input the data.
# 1. Read the "T1 Sales Data.csv" into test.sales.data dataframe
test.sales.data<-read.csv(file.choose(), header=TRUE, sep=",")

# II. Explore the data
# 1. Examine data types, check if these are detected correctly.
str(test.sales.data)
# 2. Summarize the dataset.
summary(test.sales.data) 
# 3. Plot the total market sales against test sales
plot(Sales ~ Test, data=test.sales.data)
plot(log(Sales) ~ Test, data=test.sales.data)
# 4. Plot the log-linear relation


# III. Single regression
# 1. Regress Sales on Test.
fit.s <- lm(Sales~Test, data=test.sales.data) #run a multiple linear regression model (lm) on the training data, call it "fit" 

# 2. Look at the regression summary.
summary(fit.s)
# 3. Plot the trendline.
plot(Sales ~ Test, data=test.sales.data)
abline(fit.s, col = "red")
# 4. Plot the diagnostics.
par(mfrow=c(2, 2))
plot(fit.s)
# 5. Create a dataframe for prediction.
for.prediction <- data.frame(Test = c(1,2,3,4,5,6,7,8,9,10))
# 6. Predict the Sales values.
predict(fit.s, for.prediction)

# IV. Multiple regression
# 1. Regress Sales on Test and SKU.
fit.m <- lm(Sales~Test + SKU, data=test.sales.data)
# 2. Look at the summary.
summary(fit.m)
# 3. Plot the diagnostics.
plot(fit.m)
# 4. Create a dataframe for prediction.
for.prediction.m <- data.frame(Test = c(10), SKU = c("Bear"))
# 5. Predict the Sales values
predict(fit.m, for.prediction.m)
