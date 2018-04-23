
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")
library(rpart.plot)
# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

# There are two families of CART algorithms: conditoinal interence trees (ctree function; caret package) and recursive partitioning (rpart function; partykit package)

# CTREE 

ctree_tree<-ctree(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                   PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                   BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                   data=training ) #Run ctree on training data
##ctree_tree<-ctree(default.payment.next.month~.,data=training) #Run ctree on training data - runs out of memory
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)
#str(training)

#VII. Find predicitons on testing set

ctree_prediction<-predict(ctree_tree,newdata=testing, type="response") #Predict classification (for confusion matrix); default with ctree
confusionMatrix(ctree_prediction,testing$default.payment.next.month) #Display confusion matrix

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_prediction,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

credit.default.logistic_point_testing <- predict(ctree_tree,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

#########################################################################
# RPART1
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp1 = rpart.control(cp = 0.0025)

rpart_tree1<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                  +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                  data=training, method="class", control=CART_cp1) #Run ctree on training data

printcp(rpart_tree1) # Understand the relationship between the error and cp
plotcp(rpart_tree1) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree1)

rpart_prediction_class1<-predict(rpart_tree1,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class1,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing <-predict(rpart_tree1,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class1,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree1,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing


###################################################

# RPART2
CART_cp2 = rpart.control(cp = 0.00068)

rpart_tree2<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                  +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                  data=training, method="class", control=CART_cp2) #Run ctree on training data

printcp(rpart_tree2) # Understand the relationship between the error and cp
plotcp(rpart_tree2) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree2)

rpart_prediction_class2<-predict(rpart_tree2,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class2,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing2 <-predict(rpart_tree2,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing2 <- prediction(rpart_probabilities_testing2[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing2 <- performance(rpart_pred_testing2,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing2) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing2,"auc") #Create AUC data
rpart_auc_testing2 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing2 #Display AUC value

plotLift(rpart_prediction_class2,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree2,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

###################################################

# RPART3
CART_cp3 = rpart.control(cp = 0.0001)

rpart_tree3<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                   data=training, method="class", control=CART_cp3) #Run ctree on training data

printcp(rpart_tree3) # Understand the relationship between the error and cp
plotcp(rpart_tree3) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree3)

##Check overfit
rpart_prediction_class3<-predict(rpart_tree3,newdata=training, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,training$default.payment.next.month) #Display confusion matrix


rpart_prediction_class3<-predict(rpart_tree3,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing3 <-predict(rpart_tree3,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing3 <- prediction(rpart_probabilities_testing3[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing3 <- performance(rpart_pred_testing3,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing3) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing3,"auc") #Create AUC data
rpart_auc_testing3 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing3 #Display AUC value

plotLift(rpart_prediction_class3,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#Point prediction
credit.default.logistic_point_testing <- predict(rpart_tree3,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing


######################################################################
######################################################################
######################################################################
######################################################################
#NEW VARIABLES


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")
library(rpart.plot)
# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

# There are two families of CART algorithms: conditoinal interence trees (ctree function; caret package) and recursive partitioning (rpart function; partykit package)

# CTREE 

ctree_tree<-ctree(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                  +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                  + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                    BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                    REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                    REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                    MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                    MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                  data=training ) #Run ctree on training data
##ctree_tree<-ctree(default.payment.next.month~.,data=training) #Run ctree on training data - runs out of memory
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)
#str(training)

#VII. Find predicitons on testing set

ctree_prediction<-predict(ctree_tree,newdata=testing, type="response") #Predict classification (for confusion matrix); default with ctree
confusionMatrix(ctree_prediction,testing$default.payment.next.month) #Display confusion matrix

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_prediction,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

credit.default.logistic_point_testing <- predict(ctree_tree,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

#########################################################################
# RPART1
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp1 = rpart.control(cp = 0.0025)

rpart_tree1<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                   + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                     BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                     REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                     REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                     MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp1) #Run ctree on training data

printcp(rpart_tree1) # Understand the relationship between the error and cp
plotcp(rpart_tree1) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree1)

rpart_prediction_class1<-predict(rpart_tree1,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class1,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing <-predict(rpart_tree1,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class1,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree1,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing


###################################################

# RPART2
CART_cp2 = rpart.control(cp = 0.00068)

rpart_tree2<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                   + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                     BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                     REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                     REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                     MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp2) #Run ctree on training data

printcp(rpart_tree2) # Understand the relationship between the error and cp
plotcp(rpart_tree2) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree2)

rpart_prediction_class2<-predict(rpart_tree2,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class2,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing2 <-predict(rpart_tree2,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing2 <- prediction(rpart_probabilities_testing2[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing2 <- performance(rpart_pred_testing2,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing2) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing2,"auc") #Create AUC data
rpart_auc_testing2 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing2 #Display AUC value

plotLift(rpart_prediction_class2,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree2,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

###################################################

# RPART3
CART_cp3 = rpart.control(cp = 0.0001)

rpart_tree3<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp3) #Run ctree on training data

printcp(rpart_tree3) # Understand the relationship between the error and cp
plotcp(rpart_tree3) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree3)

##Check overfit
rpart_prediction_class3<-predict(rpart_tree3,newdata=training, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,training$default.payment.next.month) #Display confusion matrix


rpart_prediction_class3<-predict(rpart_tree3,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing3 <-predict(rpart_tree3,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing3 <- prediction(rpart_probabilities_testing3[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing3 <- performance(rpart_pred_testing3,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing3) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing3,"auc") #Create AUC data
rpart_auc_testing3 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing3 #Display AUC value

plotLift(rpart_prediction_class3,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#Point prediction
credit.default.logistic_point_testing <- predict(rpart_tree3,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

######################################################################
######################################################################
######################################################################
######################################################################
#NEW VARIABLES - deleting the PAY


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")
library(rpart.plot)
# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

# There are two families of CART algorithms: conditoinal interence trees (ctree function; caret package) and recursive partitioning (rpart function; partykit package)

# CTREE 

ctree_tree<-ctree(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                    
                    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                  +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                  + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                    BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                    REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                    REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                    MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                    MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                  data=training ) #Run ctree on training data
##ctree_tree<-ctree(default.payment.next.month~.,data=training) #Run ctree on training data - runs out of memory
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)
#str(training)

#VII. Find predicitons on testing set

ctree_prediction<-predict(ctree_tree,newdata=testing, type="response") #Predict classification (for confusion matrix); default with ctree
confusionMatrix(ctree_prediction,testing$default.payment.next.month) #Display confusion matrix

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_prediction,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

credit.default.logistic_point_testing <- predict(ctree_tree,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

#########################################################################
# RPART1
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp1 = rpart.control(cp = 0.0025)

rpart_tree1<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                   + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                     BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                     REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                     REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                     MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp1) #Run ctree on training data

printcp(rpart_tree1) # Understand the relationship between the error and cp
plotcp(rpart_tree1) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree1)

rpart_prediction_class1<-predict(rpart_tree1,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class1,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing <-predict(rpart_tree1,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class1,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree1,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing


###################################################

# RPART2
CART_cp2 = rpart.control(cp = 0.00068)

rpart_tree2<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   
                   + BILL_OVER_LIMIT_1 + BILL_OVER_LIMIT_2 + BILL_OVER_LIMIT_3 + 
                     BILL_OVER_LIMIT_4 + BILL_OVER_LIMIT_5 + BILL_OVER_LIMIT_6 + 
                     REPAYMENT_RATIO_1 + REPAYMENT_RATIO_2 + REPAYMENT_RATIO_3 + 
                     REPAYMENT_RATIO_4 + REPAYMENT_RATIO_5 + REPAYMENT_RATIO_6 + 
                     MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp2) #Run ctree on training data

printcp(rpart_tree2) # Understand the relationship between the error and cp
plotcp(rpart_tree2) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree2)

rpart_prediction_class2<-predict(rpart_tree2,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class2,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing2 <-predict(rpart_tree2,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing2 <- prediction(rpart_probabilities_testing2[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing2 <- performance(rpart_pred_testing2,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing2) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing2,"auc") #Create AUC data
rpart_auc_testing2 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing2 #Display AUC value

plotLift(rpart_prediction_class2,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


credit.default.logistic_point_testing <- predict(rpart_tree2,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing

###################################################

# RPART3
CART_cp3 = rpart.control(cp = 0.0001)

rpart_tree3<-rpart(default.payment.next.month~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + 
                     PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                     BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                   +PAY_AMT1 +PAY_AMT2 +PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +MONTHS_ABOVE_LIMIT + MONTHS_WITH_NEG1 + MONTHS_WITH_NEG2 + MONTHS_WITH_0 +
                     MONTHS_WITH_1 + MONTHS_WITH_2 + MONTHS_WITH_3 + MONTHS_WITH_4 + MONTHS_WITH_5 + MONTHS_WITH_6 + MONTHS_WITH_7 + MONTHS_WITH_8,
                   data=training, method="class", control=CART_cp3) #Run ctree on training data

printcp(rpart_tree3) # Understand the relationship between the error and cp
plotcp(rpart_tree3) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
rpart.plot(rpart_tree3)

##Check overfit
rpart_prediction_class3<-predict(rpart_tree3,newdata=training, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,training$default.payment.next.month) #Display confusion matrix


rpart_prediction_class3<-predict(rpart_tree3,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class3,testing$default.payment.next.month) #Display confusion matrix

rpart_probabilities_testing3 <-predict(rpart_tree3,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing3 <- prediction(rpart_probabilities_testing3[,2], testing$default.payment.next.month) #Calculate errors
rpart_ROC_testing3 <- performance(rpart_pred_testing3,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing3) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing3,"auc") #Create AUC data
rpart_auc_testing3 <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing3 #Display AUC value

plotLift(rpart_prediction_class3,  testing$default.payment.next.month, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#Point prediction
credit.default.logistic_point_testing <- predict(rpart_tree3,newdata=Point_testing,type="prob")
credit.default.logistic_point_testing


