library(caret)
library(rJava)
library(mlr)
library(lattice)
library(ggplot2)
library(FSelector)
library(ROCR)

set.seed(50)

#create task for train and test sets
tr_Task <- makeClassifTask(data = f_tr,target = "income_level")
test_Task <- makeClassifTask(data = f_test,target = "income_level")

#Variables Importance to Target Variable:
variable_Imp <- generateFilterValuesData(tr_Task,method = "information.gain")
plotFilterValues(variable_Imp)

#Sampling in order to make the data balanced
tr_Under <- undersample(tr_Task,rate = 0.1) #keep only 10% of majority class

#oversampling
tr_Over <- oversample(tr_Task,rate=15) #multiply minority class 15 times

#Validation Strategy
res_Mod <- makeResampleDesc("CV",iters=4,stratify = TRUE)


#Logistic Reg Model:
logR_learner <- makeLearner("classif.logreg",predict.type = "prob")

logRCV_Task<- resample(logR_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
logRCV_Under<- resample(logR_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
logRCV_Over<- resample(logR_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))

AgglogRCV_Task<- logRCV_Task$aggr
AgglogRCV_Under<- logRCV_Under$aggr
AgglogRCV_Over<- logRCV_Over$aggr

#remove resample objects to save memory
remove(logRCV_Task,logRCV_Over,logRCV_Under)

#for the original and over task we got the below warnings:
#Warning messages:
#1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#2: In predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type ==  :prediction from a rank-deficient fit may be misleading

#All the three models have almost the same AUC but:
#It seems we have overfitting case for the original task, it tends to predict the majority class very good, while it has very poor reslut for the minority
#over and under sampling seem to have more balanced reslut 


#Train & Prediction
logR_Mod <- train(logR_learner, tr_Over)
logR_Pre <- predict(logR_Mod,test_Task)


#Random Forest Model
randF_learner <- makeLearner("classif.h2o.randomForest",predict.type = "prob")


#cross function
randFCV_Task<- resample(randF_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
randFCV_Under<- resample(randF_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
randFCV_Over<- resample(randF_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))

#Summary of CV
AggrandFCV_Task<-randFCV_Task$aggr
AggrandFCV_Under<-randFCV_Under$aggr
AggrandFCV_Over<-randFCV_Over$aggr

remove(randFCV_Over,randFCV_Task,randFCV_Under)

# as in logistic reg. original task perform bad for minority class, over task has very high 
#auc 0.99 while under task has a high auc with 0.94
#I am gonna train both under and over tasks becuase i think over may be overfit model


#Train & Prediction
randF_Mod <- train(randF_learner, tr_Over)
randF_Pre <- predict(randF_Mod,test_Task)


randF_Mod1 <- train(randF_learner, tr_Under)
randF_Pre1 <- predict(randF_Mod1,test_Task)

mlr::performance(randF_Pre,list(auc,acc,tpr,tnr,fpr,fp,fn))
mlr::performance(randF_Pre1,list(auc,acc,tpr,tnr,fpr,fp,fn))


#ROC
randFROCR_Pre <- asROCRPrediction(randF_Pre)
randFROCR_PreP <- ROCR::performance(randFROCR_Pre,"tpr","fpr")
randFROCR_Pre1 <- asROCRPrediction(randF_Pre1)
randFROCR_PreP1 <- ROCR::performance(randFROCR_Pre1,"tpr","fpr")


ROCR::plot(randFROCR_PreP,col = "red")
ROCR::plot(randFROCR_PreP1,col = "blue",add = TRUE)
legend("bottomright",legend = c("Over","Under"),lty=1, col=c("red","blue"))


#naive model
naiveBa_learner <- makeLearner("classif.naiveBayes",predict.type = "prob")

naiveBaCV_Task<- resample(naiveBa_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
naiveBaCV_Under<- resample(naiveBa_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
naiveBaCV_Over<- resample(naiveBa_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))

naiveBaCV_Task$aggr
naiveBaCV_Under$aggr
naiveBaCV_Over$aggr


#Train & Prediction
naiveBa_Mod <- train(naiveBa_learner, tr_Over)
naiveBa_Pre <- predict(naiveBa_Mod,test_Task)

naiveBa_Mod1 <- train(naiveBa_learner, tr_Under)
naiveBa_Pre1 <- predict(naiveBa_Mod1,test_Task)


mlr::performance(naiveBa_Pre,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
mlr::performance(naiveBa_Pre1,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))


naiveROCR_Pre <- asROCRPrediction(naiveBa_Pre)
naiveROCR_PreP <- ROCR::performance(naiveROCR_Pre,"tpr","fpr")

naiveROCR_Pre1 <- asROCRPrediction(naiveBa_Pre1)
naiveROCR_PreP1 <- ROCR::performance(naiveROCR_Pre1,"tpr","fpr")

ROCR::plot(naiveROCR_PreP,col = "red")
ROCR::plot(naiveROCR_PreP1,col = "blue",add = TRUE)
legend("bottomright",legend = c("Over","Under"),lty=1, col=c("red","blue"))


#SVM model
SVM_learner <- makeLearner("classif.svm",predict.type = "prob")

SVMCV_Task<- resample(SVM_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
SVMCV_Under<- resample(SVM_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
SVMCV_Over<- resample(SVM_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))



naiveBaCV_Task$aggr
naiveBaCV_Under$aggr
naiveBaCV_Over$aggr


#Train & Prediction
naiveBa_Mod <- train(naiveBa_learner, tr_Over)
naiveBa_Pre <- predict(naiveBa_Mod,test_Task)

performance(naiveBa_Pre,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))




#ROC Plot
randFROCR_Pre <- asROCRPrediction(randF_Pre)
randFROCR_Pre1 <- ROCR::performance(randFROCR_Pre,"tpr","fpr")

naiveROCR_Pre <- asROCRPrediction(naiveBa_Pre)
naiveROCR_Pre1 <- ROCR::performance(naiveROCR_Pre,"tpr","fpr")

ROCR::plot(randFROCR_Pre1,col = "red")
ROCR::plot(naiveROCR_Pre1,col = "blue",add = TRUE)
legend("bottomright",legend = c("randomForest","naiveBayes"),lty=1, col=c("red","blue"))

