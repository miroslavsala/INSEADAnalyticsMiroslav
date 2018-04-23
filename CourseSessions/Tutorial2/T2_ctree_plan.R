##The idea of this code is to prepare code to be used in the class

# Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")} 

#Check, and if needed install the necessary packages
pacman::p_load("caret","ROCR","lift","glmnet","MASS", "partykit") 

# Load the datafile to R

#STCdata <- read.csv("0506 STC(A) data_numerical dates.csv")
#STCdata_extra<-read.csv("0506 STC(B) data_numerical dates.csv")
STCdata<-read.csv(file.choose()) 
STCdata_extra<-read.csv(file.choose()) 

for (column in c("From.Grade", "To.Grade", "Is.Non.Annual.", "Days",
                 "CRM.Segment", "Parent.Meeting.Flag", "MDR.High.Grade",
                 "School.Sponsor", "NumberOfMeetingswithParents", "SingleGradeTripFlag",
                 "Retained.in.2012."
))
{
  STCdata[[column]] <- as.factor(STCdata[[column]])
}

str(STCdata)
str(STCdata_extra)

# merge the data
STCdata_merged = merge(STCdata, STCdata_extra, by = 'ID')
#more lame version that duplicates ID column
STC_combined <- cbind(STCdata,STCdata_extra) 

# Creta a function to fix missing values ("NAs") and preserve the NA info as surrogate variables
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

combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

STCdata_merged<-fixNAs(STCdata_merged)
STCdata_merged<-combinerarecategories(STCdata_merged, 10)

# Trim training to few hundred datapoints for things to run faster
set.seed(77850)
inTrain <- createDataPartition(y = STCdata_merged$Retained.in.2012.,
                               p = 888/2389, list = FALSE)
training <- STCdata_merged[ inTrain,]
testing <- STCdata_merged[ -inTrain,]


# Fit ctree to training data

ctree_tree<-ctree(Retained.in.2012.~.,data=training) #Run ctree on training data


# Plotting the tree
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

# Display confusion matrix

ctree_prediction<-predict(ctree_tree,newdata=testing, type="response") #Predict classification (for confusion matrix); default with ctree
confusionMatrix(ctree_prediction,testing$Retained.in.2012.) #Display confusion matrix

# ROC curve

ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$Retained.in.2012.) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_prediction,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart
