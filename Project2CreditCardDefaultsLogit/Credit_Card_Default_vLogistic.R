#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#I. Check installed packages and load pacman

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") 

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#II. Load datafile and check data structure

credit.default.rawdata<-read.csv(file.choose())
str(credit.default.rawdata)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#III. Fix incorrectly classified data types (from numerical/integer to categories - i.e. factor)

credit.default.rawdata$EDUCATION <- as.numeric(credit.default.rawdata$EDUCATION)

credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 0, 7)
credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 5, 7)
credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 6, 7)

credit.default.rawdata$ID <- as.factor(credit.default.rawdata$ID)
credit.default.rawdata$SEX <- as.factor(credit.default.rawdata$SEX)
credit.default.rawdata$EDUCATION <- as.factor(credit.default.rawdata$EDUCATION)
credit.default.rawdata$MARRIAGE <- as.factor(credit.default.rawdata$MARRIAGE)
credit.default.rawdata$PAY_0 <- as.factor(credit.default.rawdata$PAY_0)
credit.default.rawdata$PAY_2 <- as.factor(credit.default.rawdata$PAY_2)
credit.default.rawdata$PAY_3 <- as.factor(credit.default.rawdata$PAY_3)
credit.default.rawdata$PAY_4 <- as.factor(credit.default.rawdata$PAY_4)
credit.default.rawdata$PAY_5 <- as.factor(credit.default.rawdata$PAY_5)
credit.default.rawdata$PAY_6 <- as.factor(credit.default.rawdata$PAY_6)
credit.default.rawdata$default.payment.next.month <- as.factor(credit.default.rawdata$default.payment.next.month)
str(credit.default.rawdata)

#------------------------------------------------------------------------
#Exclude outliers and the guys we are testing on

#Excl_IDs <- c(25001,26001,25144,1895)
Excl_IDs <- c(25001,26001)
Point_testing <- credit.default.rawdata[c(25001,26001),]
credit.default.rawdata <- credit.default.rawdata[-Excl_IDs,] 


#------------------------------------------------------------------------

#IV. Create testing and training samples

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = credit.default.rawdata$default.payment.next.month,p = 25000/29998, list = FALSE)
training <- credit.default.rawdata[ inTrain,]
testing <- credit.default.rawdata[ -inTrain,]

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#V. Run logistic model

#credit.default.model_logistic <- glm(default.payment.next.month~ ., data=training, family="binomial"(link="logit")) 
# logistic model does not work as vector is too large "Error: cannot allocate vector of size 4.7 Gb"

credit.default.model_logistic <- glm(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                                       PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                                       BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                                       +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                                     , data=training, family="binomial"(link="logit")) 

summary(credit.default.model_logistic)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#VI. Run stepwise regression

credit.default.model_logistic_stepwiseAIC<-stepAIC(credit.default.model_logistic,direction = c("both"),trace = 1)
summary(credit.default.model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(credit.default.model_logistic_stepwiseAIC) # Normal Q-Q with some problems
par(mfrow=c(1,1))

credit.default.model_logistic_FINAL <- credit.default.model_logistic_stepwiseAIC #Final model

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#VII. Find predicitons on testing set
credit.default.logistic_probabilities_testing <- predict(credit.default.model_logistic_FINAL,newdata=testing,type="response")
credit.default.logistic_pred_testing <- rep("1",4997)
credit.default.logistic_pred_testing[credit.default.logistic_probabilities_testing<6636/30000]="0" 
confusionMatrix(credit.default.logistic_pred_testing,testing$default.payment.next.month) 

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#VIII. Build ROC Curve
credit.default.logistic_ROC_pred <- prediction(credit.default.logistic_probabilities_testing, testing$default.payment.next.month)
credit.default.logistic_ROC_testing <- performance(credit.default.logistic_ROC_pred,"tpr","fpr")
plot(credit.default.logistic_ROC_testing)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#IX. Compute AUC
credit.default.auc.tmp <- performance(credit.default.logistic_ROC_pred,"auc")
credit.default.logistic_auc_testing <- as.numeric(credit.default.auc.tmp@y.values)
credit.default.logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#X. Build lift chart
plotLift(credit.default.logistic_probabilities_testing, testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10)

#this gives me probabilities (based on calculated utilities)


#XI. Find predicitons on point set


credit.default.logistic_point_testing <- predict(credit.default.model_logistic_FINAL,newdata=Point_testing,type="response")
credit.default.logistic_point_testing


#copy 2 rows simultaneously 
#Point_testing_both <- rbind(Point_testing,Point_testing2)




#ASSIGNMENT NR 6 - new variables
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#------------------------------------------------------------------------

#I. Check installed packages and load pacman

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") 

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#II. Load datafile and check data structure

credit.default.rawdata<-read.csv(file.choose())

#------------------------------------------------------------------------

# CREATE NEW VARIABLES

## RATIO BETWEEN BILL AMOUNT AND MAXIMUM BALANCE ALLOWED (IF NEGATIVE RETURN ZERO)
credit.default.rawdata$BILL_OVER_LIMIT_1 <- ifelse(credit.default.rawdata$BILL_AMT1 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT1 / credit.default.rawdata$LIMIT_BAL)
credit.default.rawdata$BILL_OVER_LIMIT_2 <- ifelse(credit.default.rawdata$BILL_AMT2 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT2 / credit.default.rawdata$LIMIT_BAL)
credit.default.rawdata$BILL_OVER_LIMIT_3 <- ifelse(credit.default.rawdata$BILL_AMT3 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT3 / credit.default.rawdata$LIMIT_BAL)
credit.default.rawdata$BILL_OVER_LIMIT_4 <- ifelse(credit.default.rawdata$BILL_AMT4 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT4 / credit.default.rawdata$LIMIT_BAL)
credit.default.rawdata$BILL_OVER_LIMIT_5 <- ifelse(credit.default.rawdata$BILL_AMT5 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT5 / credit.default.rawdata$LIMIT_BAL)
credit.default.rawdata$BILL_OVER_LIMIT_6 <- ifelse(credit.default.rawdata$BILL_AMT6 / credit.default.rawdata$LIMIT_BAL < 0,0,credit.default.rawdata$BILL_AMT6 / credit.default.rawdata$LIMIT_BAL)

## REPAYMENT RATIO (IF ERROR RETURN ZERO)
### SEPTEMBER
credit.default.rawdata$REPAYMENT_RATIO_1 <- credit.default.rawdata$PAY_AMT1 / credit.default.rawdata$BILL_AMT1
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_1)
credit.default.rawdata$REPAYMENT_RATIO_1 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT1 / credit.default.rawdata$BILL_AMT1)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_1)
credit.default.rawdata$REPAYMENT_RATIO_1 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_1)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_1 < 0
credit.default.rawdata$REPAYMENT_RATIO_1 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_1)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_1)
credit.default.rawdata$REPAYMENT_RATIO_1 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_1)

### AUGUST
credit.default.rawdata$REPAYMENT_RATIO_2 <- credit.default.rawdata$PAY_AMT2 / credit.default.rawdata$BILL_AMT2
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_1)
credit.default.rawdata$REPAYMENT_RATIO_2 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT2 / credit.default.rawdata$BILL_AMT2)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_2)
credit.default.rawdata$REPAYMENT_RATIO_2 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_2)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_2 < 0
credit.default.rawdata$REPAYMENT_RATIO_2 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_2)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_2)
credit.default.rawdata$REPAYMENT_RATIO_2 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_2)

### JULY
credit.default.rawdata$REPAYMENT_RATIO_3 <- credit.default.rawdata$PAY_AMT3 / credit.default.rawdata$BILL_AMT3
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_3)
credit.default.rawdata$REPAYMENT_RATIO_3 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT3 / credit.default.rawdata$BILL_AMT3)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_3)
credit.default.rawdata$REPAYMENT_RATIO_3 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_3)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_3 < 0
credit.default.rawdata$REPAYMENT_RATIO_3 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_3)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_3)
credit.default.rawdata$REPAYMENT_RATIO_3 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_3)

### JUNE
credit.default.rawdata$REPAYMENT_RATIO_4 <- credit.default.rawdata$PAY_AMT4 / credit.default.rawdata$BILL_AMT4
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_4)
credit.default.rawdata$REPAYMENT_RATIO_4 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT4 / credit.default.rawdata$BILL_AMT4)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_4)
credit.default.rawdata$REPAYMENT_RATIO_4 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_4)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_4 < 0
credit.default.rawdata$REPAYMENT_RATIO_4 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_4)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_4)
credit.default.rawdata$REPAYMENT_RATIO_4 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_4)

### MAY
credit.default.rawdata$REPAYMENT_RATIO_5 <- credit.default.rawdata$PAY_AMT5 / credit.default.rawdata$BILL_AMT5
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_5)
credit.default.rawdata$REPAYMENT_RATIO_5 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT5 / credit.default.rawdata$BILL_AMT5)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_5)
credit.default.rawdata$REPAYMENT_RATIO_5 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_5)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_5 < 0
credit.default.rawdata$REPAYMENT_RATIO_5 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_5)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_5)
credit.default.rawdata$REPAYMENT_RATIO_5 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_5)

### APRIL
credit.default.rawdata$REPAYMENT_RATIO_6 <- credit.default.rawdata$PAY_AMT6 / credit.default.rawdata$BILL_AMT6
cond_NaN <- is.nan(credit.default.rawdata$REPAYMENT_RATIO_6)
credit.default.rawdata$REPAYMENT_RATIO_6 <- ifelse(cond_NaN,1,credit.default.rawdata$PAY_AMT6 / credit.default.rawdata$BILL_AMT6)
cond_infinite <- is.infinite(credit.default.rawdata$REPAYMENT_RATIO_6)
credit.default.rawdata$REPAYMENT_RATIO_6 <- ifelse(cond_infinite,1,credit.default.rawdata$REPAYMENT_RATIO_6)
cond_negative <- credit.default.rawdata$REPAYMENT_RATIO_6 < 0
credit.default.rawdata$REPAYMENT_RATIO_6 <- ifelse(cond_negative,1,credit.default.rawdata$REPAYMENT_RATIO_6)
cond_NA <- is.na(credit.default.rawdata$REPAYMENT_RATIO_6)
credit.default.rawdata$REPAYMENT_RATIO_6 <- ifelse(cond_NA,1,credit.default.rawdata$REPAYMENT_RATIO_6)

## NUMBER OF MONTHS ABOVE BALANCE LIMIT

cond_SEP <- credit.default.rawdata$BILL_OVER_LIMIT_1 > 1
cond_AUG <- credit.default.rawdata$BILL_OVER_LIMIT_2 > 1
cond_JUL <- credit.default.rawdata$BILL_OVER_LIMIT_3 > 1
cond_JUN <- credit.default.rawdata$BILL_OVER_LIMIT_4 > 1
cond_MAY <- credit.default.rawdata$BILL_OVER_LIMIT_5 > 1
cond_APR <- credit.default.rawdata$BILL_OVER_LIMIT_6 > 1

credit.default.rawdata$MONTHS_ABOVE_LIMIT <- ifelse(cond_SEP,1,0) + ifelse(cond_AUG,1,0) + ifelse(cond_JUL,1,0) + ifelse(cond_JUN,1,0) + ifelse(cond_MAY,1,0) + ifelse(cond_APR,1,0)

## COUNT OF THE MONTH IN DEFAULT

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < -1
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < -1 
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < -1
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < -1
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < -1
cond_APR_PAY<-credit.default.rawdata$PAY_6 < -1

credit.default.rawdata$MONTHS_WITH_NEG2 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 0 & credit.default.rawdata$PAY_0 > -2
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 0 & credit.default.rawdata$PAY_2 > -2
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 0 & credit.default.rawdata$PAY_3 > -2
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 0 & credit.default.rawdata$PAY_4 > -2
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 0 & credit.default.rawdata$PAY_5 > -2
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 0 & credit.default.rawdata$PAY_6 > -2

credit.default.rawdata$MONTHS_WITH_NEG1 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 1 & credit.default.rawdata$PAY_0 > -1
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 1 & credit.default.rawdata$PAY_2 > -1
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 1 & credit.default.rawdata$PAY_3 > -1
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 1 & credit.default.rawdata$PAY_4 > -1
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 1 & credit.default.rawdata$PAY_5 > -1
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 1 & credit.default.rawdata$PAY_6 > -1

credit.default.rawdata$MONTHS_WITH_0 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 2 & credit.default.rawdata$PAY_0 > 0
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 2 & credit.default.rawdata$PAY_2 > 0
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 2 & credit.default.rawdata$PAY_3 > 0
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 2 & credit.default.rawdata$PAY_4 > 0
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 2 & credit.default.rawdata$PAY_5 > 0
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 2 & credit.default.rawdata$PAY_6 > 0

credit.default.rawdata$MONTHS_WITH_1 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 3 & credit.default.rawdata$PAY_0 > 1
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 3 & credit.default.rawdata$PAY_2 > 1
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 3 & credit.default.rawdata$PAY_3 > 1
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 3 & credit.default.rawdata$PAY_4 > 1
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 3 & credit.default.rawdata$PAY_5 > 1
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 3 & credit.default.rawdata$PAY_6 > 1

credit.default.rawdata$MONTHS_WITH_2 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 4 & credit.default.rawdata$PAY_0 > 2
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 4 & credit.default.rawdata$PAY_2 > 2
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 4 & credit.default.rawdata$PAY_3 > 2
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 4 & credit.default.rawdata$PAY_4 > 2
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 4 & credit.default.rawdata$PAY_5 > 2
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 4 & credit.default.rawdata$PAY_6 > 2

credit.default.rawdata$MONTHS_WITH_3 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 5 & credit.default.rawdata$PAY_0 > 3
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 5 & credit.default.rawdata$PAY_2 > 3
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 5 & credit.default.rawdata$PAY_3 > 3
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 5 & credit.default.rawdata$PAY_4 > 3
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 5 & credit.default.rawdata$PAY_5 > 3
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 5 & credit.default.rawdata$PAY_6 > 3

credit.default.rawdata$MONTHS_WITH_4 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 6 & credit.default.rawdata$PAY_0 > 4
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 6 & credit.default.rawdata$PAY_2 > 4
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 6 & credit.default.rawdata$PAY_3 > 4
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 6 & credit.default.rawdata$PAY_4 > 4
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 6 & credit.default.rawdata$PAY_5 > 4
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 6 & credit.default.rawdata$PAY_6 > 4

credit.default.rawdata$MONTHS_WITH_5 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 7 & credit.default.rawdata$PAY_0 > 5
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 7 & credit.default.rawdata$PAY_2 > 5
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 7 & credit.default.rawdata$PAY_3 > 5
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 7 & credit.default.rawdata$PAY_4 > 5
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 7 & credit.default.rawdata$PAY_5 > 5
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 7 & credit.default.rawdata$PAY_6 > 5

credit.default.rawdata$MONTHS_WITH_6 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 < 8 & credit.default.rawdata$PAY_0 > 6
cond_AUG_PAY<-credit.default.rawdata$PAY_2 < 8 & credit.default.rawdata$PAY_2 > 6
cond_JUL_PAY<-credit.default.rawdata$PAY_3 < 8 & credit.default.rawdata$PAY_3 > 6
cond_JUN_PAY<-credit.default.rawdata$PAY_4 < 8 & credit.default.rawdata$PAY_4 > 6
cond_MAY_PAY<-credit.default.rawdata$PAY_5 < 8 & credit.default.rawdata$PAY_5 > 6
cond_APR_PAY<-credit.default.rawdata$PAY_6 < 8 & credit.default.rawdata$PAY_6 > 6

credit.default.rawdata$MONTHS_WITH_7 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

cond_SEP_PAY<-credit.default.rawdata$PAY_0 > 7
cond_AUG_PAY<-credit.default.rawdata$PAY_2 > 7
cond_JUL_PAY<-credit.default.rawdata$PAY_3 > 7
cond_JUN_PAY<-credit.default.rawdata$PAY_4 > 7
cond_MAY_PAY<-credit.default.rawdata$PAY_5 > 7
cond_APR_PAY<-credit.default.rawdata$PAY_6 > 7

credit.default.rawdata$MONTHS_WITH_8 <- ifelse(cond_SEP_PAY,1,0) + ifelse(cond_AUG_PAY,1,0) + ifelse(cond_JUL_PAY,1,0) + ifelse(cond_JUN_PAY,1,0) + ifelse(cond_MAY_PAY,1,0) + ifelse(cond_APR_PAY,1,0)

### DELTA OF BILL OUTSTANDING

credit.default.rawdata$DELTA_LAST_MONTH <- credit.default.rawdata$BILL_AMT1-credit.default.rawdata$BILL_AMT2
credit.default.rawdata$DELTA_LAST_2MONTHS <- credit.default.rawdata$BILL_AMT1-credit.default.rawdata$BILL_AMT3
credit.default.rawdata$DELTA_LAST_3MONTHS <- credit.default.rawdata$BILL_AMT1-credit.default.rawdata$BILL_AMT4


#------------------------------------------------------------------------

#III. Fix incorrectly classified data types (from numerical/integer to categories)

credit.default.rawdata$EDUCATION <- as.numeric(credit.default.rawdata$EDUCATION)
credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 0, 7)
credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 5, 7)
credit.default.rawdata$EDUCATION <- replace(credit.default.rawdata$EDUCATION, credit.default.rawdata$EDUCATION == 6, 7)
credit.default.rawdata$ID <- as.factor(credit.default.rawdata$ID)
credit.default.rawdata$SEX <- as.factor(credit.default.rawdata$SEX)
credit.default.rawdata$EDUCATION <- as.factor(credit.default.rawdata$EDUCATION)
credit.default.rawdata$MARRIAGE <- as.factor(credit.default.rawdata$MARRIAGE)
credit.default.rawdata$PAY_0 <- as.factor(credit.default.rawdata$PAY_0)
credit.default.rawdata$PAY_2 <- as.factor(credit.default.rawdata$PAY_2)
credit.default.rawdata$PAY_3 <- as.factor(credit.default.rawdata$PAY_3)
credit.default.rawdata$PAY_4 <- as.factor(credit.default.rawdata$PAY_4)
credit.default.rawdata$PAY_5 <- as.factor(credit.default.rawdata$PAY_5)
credit.default.rawdata$PAY_6 <- as.factor(credit.default.rawdata$PAY_6)
credit.default.rawdata$default.payment.next.month <- as.factor(credit.default.rawdata$default.payment.next.month)
str(credit.default.rawdata)

#------------------------------------------------------------------------
#Exclude outliers and the guys we are testing on

Point_testing <- credit.default.rawdata[c(25001,26001),]
Excl_IDs <- c(25001,26001)
credit.default.rawdata <- credit.default.rawdata[-Excl_IDs,] 


#------------------------------------------------------------------------

#IV. Create testing and training samples

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = credit.default.rawdata$default.payment.next.month,p = 25000/29998, list = FALSE)
training <- credit.default.rawdata[ inTrain,]
testing <- credit.default.rawdata[ -inTrain,]

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#V. Run logistic model

#credit.default.model_logistic <- glm(default.payment.next.month~ ., data=training, family="binomial"(link="logit")) 
# logistic model does not work as vector is too large "Error: cannot allocate vector of size 4.7 Gb"

credit.default.model_logistic <- glm(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                                       PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                                       BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                                     +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                                     + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                                       BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                                       REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                                       REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                                       MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                                       MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8
                                     , data=training, family="binomial"(link="logit")) 

summary(credit.default.model_logistic)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#VI. Run stepwise regression

credit.default.model_logistic_stepwiseAIC<-stepAIC(credit.default.model_logistic,direction = c("both"),trace = 1)
summary(credit.default.model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(credit.default.model_logistic_stepwiseAIC) # Normal Q-Q with some problems
par(mfrow=c(1,1))

credit.default.model_logistic_FINAL <- credit.default.model_logistic_stepwiseAIC #Final model

#------------------------------------------------------------------------
#------------------------------------------------------------------------




#VII. Find predicitons on testing set
credit.default.logistic_probabilities_testing <- predict(credit.default.model_logistic_FINAL,newdata=testing,type="response")
credit.default.logistic_pred_testing <- rep("1",4997)
credit.default.logistic_pred_testing[credit.default.logistic_probabilities_testing<6636/30000]="0" 
confusionMatrix(credit.default.logistic_pred_testing,testing$default.payment.next.month) 

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#VIII. Build ROC Curve
credit.default.logistic_ROC_pred <- prediction(credit.default.logistic_probabilities_testing, testing$default.payment.next.month)
credit.default.logistic_ROC_testing <- performance(credit.default.logistic_ROC_pred,"tpr","fpr")
plot(credit.default.logistic_ROC_testing)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#IX. Compute AUC
credit.default.auc.tmp <- performance(credit.default.logistic_ROC_pred,"auc")
credit.default.logistic_auc_testing <- as.numeric(credit.default.auc.tmp@y.values)
credit.default.logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#X. Build lift chart
plotLift(credit.default.logistic_probabilities_testing, testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10)

#this gives me probabilities (based on calculated utilities)


#XI. Find predicitons on point set

credit.default.logistic_point_testing <- predict(credit.default.model_logistic_FINAL,newdata=Point_testing,type="response")
credit.default.logistic_point_testing


#copy 2 rows simultaneously 
#Point_testing_both <- rbind(Point_testing,Point_testing2)

##########################testing on "testing" minus the one Pay_2 = 8

write.csv(testing, file = "testing.csv") # export the selected model's predictions into a CSV file
Excl_IDs_odds <- 1490
testing_ex_odds <- testing[-Excl_IDs_odds,]


credit.default.logistic_probabilities_testing <- predict(credit.default.model_logistic_FINAL,newdata=testing_ex_odds,type="response")
credit.default.logistic_pred_testing <- rep("1",4996)
credit.default.logistic_pred_testing[credit.default.logistic_probabilities_testing<6636/30000]="0" 
confusionMatrix(credit.default.logistic_pred_testing,testing_ex_odds$default.payment.next.month) 

#------------------------------------------------------------------------

#VIII. Build ROC Curve
credit.default.logistic_ROC_pred <- prediction(credit.default.logistic_probabilities_testing, testing_ex_odds$default.payment.next.month)
credit.default.logistic_ROC_testing <- performance(credit.default.logistic_ROC_pred,"tpr","fpr")
plot(credit.default.logistic_ROC_testing)

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#IX. Compute AUC
credit.default.auc.tmp <- performance(credit.default.logistic_ROC_pred,"auc")
credit.default.logistic_auc_testing <- as.numeric(credit.default.auc.tmp@y.values)
credit.default.logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#------------------------------------------------------------------------
#------------------------------------------------------------------------

#X. Build lift chart
plotLift(credit.default.logistic_probabilities_testing, testing_ex_odds$default.payment.next.month, cumulative = TRUE, n.buckets = 10)

#this gives me probabilities (based on calculated utilities)


#XI. Find predicitons on point set

credit.default.logistic_point_testing <- predict(credit.default.model_logistic_FINAL,newdata=Point_testing,type="response")
credit.default.logistic_point_testing

