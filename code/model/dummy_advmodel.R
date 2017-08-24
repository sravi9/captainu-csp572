library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
library(MASS)
library(caret)
library(ROCR)
library(e1071)
library(dummies)
library(glmnet)
analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
january<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
march<-analysisdata[which(analysisdata[,"Timetag"]=="2016/3/1"),]
october<-analysisdata[which(analysisdata[,"Timetag"]=="2015/10/1"),]
november<-analysisdata[which(analysisdata[,"Timetag"]=="2015/11/1"),]
december<-analysisdata[which(analysisdata[,"Timetag"]=="2015/12/1"),]

january<-january[,c(-1,-2,-4,-5,-6,-24,-25,-70,-71,-73,-74,-75,-76)]
#january<-january[,c(-1,-2,-4,-5,-6,-25,-68)]
names(january)
january[,61]<-as.factor(january[,61])
january[,62]<-as.factor(january[,62])
january[,63]<-as.factor(january[,63])
january[,64]<-as.factor(january[,64])
january[,65]<-as.factor(january[,65])
january[,66]<-as.factor(january[,66])


str(january)
nrow(january)
prechurn<-which(january[,61]=="1")
length(prechurn)

dummyjanuary<-dummy.data.frame(data = january, names = c("gender","act","Current","Underpromotion","Ralations"))
dummyjanuary[,1:60]<-scale(dummyjanuary[,1:60])
dummyjanuary[is.na(dummyjanuary)]<-0


selejan<-dummyjanuary[,c(68,5,72,10,4,52,1,74,67,44,70,24,61)]
names(selejan)

targetVar<-"one_month_churn"
xVars<-names(selejan[,-13])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))



testchurn<-which(test_jan[,13]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

ctrl <- trainControl(method = "cv", 
                     number = 10,
                     sampling = "up")

balanced_rf <- caret::train(modelForm,
                            data = train_jan,
                            method = "rf",
                            trControl = ctrl)

Prediction_jan <- predict(balanced_rf, test_jan, type = "prob")
jan_pred <- ifelse(Prediction_jan[,2]> 0.5,1,0)
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = jan_pred)
varImp(balanced_rf)

balanced_svmweight <- caret::train(modelForm,
                                   data = train_jan,
                                   method = "svmLinearWeights",
                                   trControl = ctrl)

Prediction_jan <- predict(balanced_svmweight, test_jan)
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)
varImp(balanced_svmweight)

balanced_svmlk <- caret::train(modelForm,
                               data = train_jan,
                               method = "svmLinear2",
                               trControl = ctrl)

Prediction_jan <- predict(balanced_svmlk, test_jan)
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)

balanced_boosting <- caret::train(modelForm,
                                  data = train_jan,
                                  method = "gbm",
                                  trControl = ctrl)

Prediction_jan <- predict(balanced_boosting, test_jan)
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)
varImp(balanced_boosting)


balanced_descent <- caret::train(modelForm,
                                 data = train_jan,
                                 method = "mlpSGD",
                                 trControl = ctrl)

Prediction_jan <- predict(balanced_boosting, test_jan)
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)
varImp(balanced_descent)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################





november<-november[,c(-1,-2,-4,-5,-6,-24,-25,-70,-71,-73,-74,-75,-76)]
names(november)
november[,61]<-as.factor(november[,61])
november[,62]<-as.factor(november[,62])
november[,63]<-as.factor(november[,63])
november[,64]<-as.factor(november[,64])
november[,65]<-as.factor(november[,65])
november[,66]<-as.factor(november[,66])


str(november)
nrow(november)
prechurn<-which(november[,61]=="1")
length(prechurn)

dummynovember<-dummy.data.frame(data = november, names = c("gender","act","Current","Underpromotion","Ralations"))
dummynovember[,1:60]<-scale(dummynovember[,1:60])
dummynovember[is.na(dummynovember)]<-0

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=dummynovember)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=dummynovember)

nove_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(dummynovember))
selenove<-dummynovember[,c(68,1,43,4,58,35,55,67,24,12,72,74,57,46,29,61)]


targetVar<-"one_month_churn"
xVars<-names(selenove[,-16])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))


trainweight<-as.vector(rep(NA,11012))
trainchurn<-which(train_nove[,16]=="1")
trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_nove[,16]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

nove_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_nove)

nove_fitted <- predict(nove_model
                       ,newdata = test_nove[,xVars]
                       # Specifying response means we want the probabilities
                       ,type='response')

nove_pred <- ifelse(nove_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = nove_pred
                             , reference = test_nove[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = nove_pred, trues = test_nove$one_month_churn)

pr <- prediction(nove_pred, test_nove$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


