
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages

STCdata<-read.csv(file.choose()) # Load the datafile to R

str(STCdata) # See if some data types were misclassified when importing data from CSV, and they are e.g. retained.in.2012 - it is not int but factor

# Fixing incorrectly classified data types:
STCdata$From.Grade <- as.factor(STCdata$From.Grade)
STCdata$To.Grade <- as.factor(STCdata$To.Grade)
STCdata$Is.Non.Annual. <- as.factor(STCdata$Is.Non.Annual.)
STCdata$Days <- as.factor(STCdata$Days)
#STCdata$Departure.Date <- as.Date(STCdata$Departure.Date, origin="1899-12-30")
#STCdata$Return.Date <- as.Date(STCdata$Return.Date, origin="1899-12-30")
#STCdata$Deposit.Date <- as.Date(STCdata$Deposit.Date, origin="1899-12-30")
#STCdata$Early.RPL <- as.Date(STCdata$Early.RPL, origin="1899-12-30")
#STCdata$Latest.RPL <- as.Date(STCdata$Latest.RPL, origin="1899-12-30")
#STCdata$Initial.System.Date <- as.Date(STCdata$Initial.System.Date, origin="1899-12-30")
STCdata$CRM.Segment <- as.factor(STCdata$CRM.Segment)
STCdata$Parent.Meeting.Flag <- as.factor(STCdata$Parent.Meeting.Flag)
STCdata$MDR.High.Grade <- as.factor(STCdata$MDR.High.Grade)
STCdata$School.Sponsor <- as.factor(STCdata$School.Sponsor)
STCdata$NumberOfMeetingswithParents <- as.factor(STCdata$NumberOfMeetingswithParents)
STCdata$SingleGradeTripFlag <- as.factor(STCdata$SingleGradeTripFlag)
#STCdata$FirstMeeting <- as.Date(STCdata$FirstMeeting, origin="1899-12-30")
#STCdata$LastMeeting <- as.Date(STCdata$LastMeeting, origin="1899-12-30")
STCdata$Retained.in.2012. <- as.factor(STCdata$Retained.in.2012.)

# Create a custom function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        } 
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }  
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }  
        }       
      }
  } 
  return(data_frame) 
}

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }


#Apply the fixNAs and combinerarecategories functions to the data and then split it into testing and training data.

STCdata<-fixNAs(STCdata)
STCdata<-combinerarecategories(STCdata,10) #combine categories with <10 values in STCdata into "Other"

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = STCdata$Retained.in.2012.,p = 1888/2389, list = FALSE)
training <- STCdata[ inTrain,]
testing <- STCdata[ -inTrain,]
#if I wanted the test in time series then I want to take the latest month data and that will be my testing set. There is code for that in R - use Google)


# Select the variables to be included in the "base-case" model
# First include all variables use glm(Retained.in.2012.~ ., data=training, family="binomial"(link="logit")) Then see which ones have "NA" in coefficients and remove those

model_logistic<-glm(Retained.in.2012.~ Special.Pay + 
                      To.Grade + Group.State + Is.Non.Annual. +
                      Tuition + FRP.Active + FRP.Cancelled + FRP.Take.up.percent. + 
                      Cancelled.Pax + Total.Discount.Pax + Initial.System.Date + 
                      Poverty.Code + CRM.Segment + School.Type + Parent.Meeting.Flag + 
                      MDR.Low.Grade + MDR.High.Grade + Total.School.Enrollment + 
                      EZ.Pay.Take.Up.Rate + School.Sponsor +  
                      SPR.New.Existing + FPP + FirstMeeting + LastMeeting + 
                      DifferenceTraveltoFirstMeeting + DepartureMonth  + MajorProgramCode + SingleGradeTripFlag + 
                      FPP.to.School.enrollment + FPP.to.PAX + SchoolSizeIndicator, data=training, family="binomial"(link="logit"))

summary(model_logistic) 

# to add surrogates paste this to the list of variables; note, it will run quite a bit slower
#Special.Pay_surrogate + Early.RPL_surrogate + Latest.RPL_surrogate + 
#Initial.System.Date_surrogate + CRM.Segment_surrogate + MDR.High.Grade_surrogate + 
#Total.School.Enrollment_surrogate + FirstMeeting_surrogate + 
#LastMeeting_surrogate + DifferenceTraveltoFirstMeeting_surrogate + 
#DifferenceTraveltoLastMeeting_surrogate + FPP.to.School.enrollment_surrogate

##The model clearly has too many variables, most of which are insignificant 

## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both".
## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise, AIC balances number of variables in the model and it's predictive power
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

model_logistic_FINAL<-model_logistic_stepwiseAIC #Final model

###Finding predicitons on Testing set
logistic_probabilities_testing<-predict(model_logistic_FINAL,newdata=testing,type="response") #Predict probabilities
logistic_pred_testing<-rep("1",500)
logistic_pred_testing[logistic_probabilities_testing<0.6073]="0" #Predict classification. Why 0.6073 - that's the average probability of being retained in the data
confusionMatrix(logistic_pred_testing,testing$Retained.in.2012.) #Display confusion matrix

####ROC Curve
logistic_ROC_pred <- prediction(logistic_probabilities_testing, testing$Retained.in.2012.)
logistic_ROC_testing <- performance(logistic_ROC_pred,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_pred,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities_testing, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

