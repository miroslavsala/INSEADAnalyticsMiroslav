ChoiceData<-read.csv(file.choose()) #load data
str(ChoiceData) #make sure that the field types are interpreted correctly (as numbers/integers, factors, etc.)
#D will be zeroed out and H = 1, the first variable is zeroed out after looking at the structure of data

#binomial model
Logistic_Model<-glm(Choice ~ GMAT, data = ChoiceData, family="binomial"(link="logit")) #logistic regression is part of the "generalized linear models" family, hence glm
summary(Logistic_Model) #summary of the model
# Intercept = a

par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
plot(Logistic_Model) # check the model using diagnostic plots

#this gives me probabilities (based on calculated utilities)
predict(Logistic_Model, newdata=data.frame("GMAT"=700),type="response") #first predicting utility, then predict the probability of choice as a function of GMAT

#this code below will give me relative utility
predict(Logistic_Model, newdata=data.frame("GMAT"=700),type="link") 
