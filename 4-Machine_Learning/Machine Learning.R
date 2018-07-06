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

tr_smote <- smote(tr_Task,rate = 10,nn = 3)



#Validation Strategy
res_Mod <- makeResampleDesc("CV",iters=5,stratify = TRUE)


#Logistic Reg Model:
logR_learner <- makeLearner("classif.logreg",predict.type = "prob", fix.factors.prediction = TRUE)


logRCV_Task<- resample(logR_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
logRCV_Under<- resample(logR_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))
logRCV_Over<- resample(logR_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr,fp,fn))

AgglogRCV_Task<- logRCV_Task$aggr
AgglogRCV_Under<- logRCV_Under$aggr
AgglogRCV_Over<- logRCV_Over$aggr


cbind(as.data.frame(AgglogRCV_Task),as.data.frame(AgglogRCV_Under),as.data.frame(AgglogRCV_Over))

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
cbind(as.data.frame(AggrandFCV_Task),as.data.frame(AggrandFCV_Under),as.data.frame(AggrandFCV_Over))

remove(randFCV_Over,randFCV_Task,randFCV_Under)

# as in logistic reg. original task perform bad for minority class, over task has very high 
#auc 0.99 while under task has a high auc with 0.94
#I am gonna train both under and over tasks becuase i think over may be overfit model


#Train & Prediction
randF_Mod_Over <- train(randF_learner, tr_Over)
randF_Pre_Over <- predict(randF_Mod,test_Task)

randF_Mod_Under <- train(randF_learner, tr_Under)
randF_Pre_Under <- predict(randF_Mod1,test_Task)

#performance measure for train models
randF_Over_Mes<- mlr::performance(randF_Pre_Over,list(auc,acc,tpr,tnr,fpr))
randF_Under_Mes<-mlr::performance(randF_Pre_Under,list(auc,acc,tpr,tnr,fpr))
cbind(as.data.frame(randF_Over_Mes),as.data.frame(randF_Under_Mes))

# Over Model Confusion Matrix
mlr::calculateConfusionMatrix(randF_Pre_Over)

# Under Model Confusion Matrix
mlr::calculateConfusionMatrix(randF_Pre_Under)

#ROC
randFROCR_Th <- generateThreshVsPerfData(list(Over = randF_Pre_Over, Under = randF_Pre_Under),
                                         measures = list(tpr,fpr,ppv) )

#ROC Curve:
randFROCR_Th <- generateThreshVsPerfData(list(Over = randF_Pre_Over, Under = randF_Pre_Under),
                                         measures = list(tpr,fpr,ppv) )
gridExtra::grid.arrange(plotROCCurves(randFROCR_Th,measures = list(fpr,tpr),diagonal = FALSE)+ggtitle("AUC") ,
                        plotROCCurves(randFROCR_Th,measures = list(tpr,ppv),diagonal = FALSE) + ggtitle("Precision vs Recall"),ncol = 2)


#naive model
naiveBa_learner <- makeLearner("classif.naiveBayes",predict.type = "prob", fix.factors.prediction = TRUE)

help(classif.naiveBayes)

naiveBaCV_Task<- resample(naiveBa_learner,tr_Task,res_Mod,measures = list(auc,acc,tpr,tnr,fpr))
naiveBaCV_Under<- resample(naiveBa_learner,tr_Under,res_Mod,measures = list(auc,acc,tpr,tnr,fpr))
naiveBaCV_Over<- resample(naiveBa_learner,tr_Over,res_Mod,measures = list(auc,acc,tpr,tnr,fpr))

AggnaiveBaCV_Task<- naiveBaCV_Task$aggr
AggnaiveBaCV_Under<- naiveBaCV_Under$aggr
AggnaiveBaCV_Over<- naiveBaCV_Over$aggr

remove(naiveBaCV_Task,naiveBaCV_Over,naiveBaCV_Under)

#Train & Prediction
naiveBa_Mod_Over <- train(naiveBa_learner, tr_Over)
naiveBa_Pre_Over <- predict(naiveBa_Mod_Over,test_Task)


naiveBa_Mod_Under <- train(naiveBa_learner, tr_Under)
naiveBa_Pre_Under <- predict(naiveBa_Mod_Under,test_Task)


#performance measure for train models
naiveBa_Over_Mes<- mlr::performance(naiveBa_Pre_Over,list(auc,acc,tpr,tnr,fpr))
naiveBa_Under_Mes<-mlr::performance(naiveBa_Pre_Under,list(auc,acc,tpr,tnr,fpr))
cbind(as.data.frame(naiveBa_Over_Mes),as.data.frame(naiveBa_Under_Mes))

# Over Model Confusion Matrix
mlr::calculateConfusionMatrix(naiveBa_Pre_Over)

# Under Model Confusion Matrix
mlr::calculateConfusionMatrix(naiveBa_Pre_Under)





#SVM model
SVM_learner <- makeLearner("classif.svm",predict.type = "prob")


SVM_learner$par.vals <- list(class.weights = c("0"=1,"1"=10),kernel="radial")

svm_param <- makeParamSet(makeIntegerParam("cost",lower = 0.1,upper = 100),
                          makeIntegerParam("gamma",lower= 0.5,upper = 2))


set_search <- makeTuneControlRandom(maxit = 5L)

svm_tune <- tuneParams(learner = SVM_learner,task = tr_Task,
                       measures = list(acc,tpr,tnr,fpr,fp,fn), 
                       par.set = svm_param,control = set_search,resampling = res_Mod)



