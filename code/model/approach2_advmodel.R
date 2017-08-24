library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
library(MASS)
library(caret)
library(ROCR)
library(e1071)
analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
january<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
october<-analysisdata[which(analysisdata[,"Timetag"]=="2015/10/1"),]
november<-analysisdata[which(analysisdata[,"Timetag"]=="2015/11/1"),]
december<-analysisdata[which(analysisdata[,"Timetag"]=="2015/12/1"),]

january<-january[,c(-1,-2,-4,-5,-6,-24,-25)]
#january<-january[,c(-1,-2,-4,-5,-6,-25,-68)]
names(january)
january[,61]<-as.factor(january[,61])
january[,63]<-as.factor(january[,63])
january[,64]<-as.factor(january[,64])
january[,65]<-as.factor(january[,65])
january[,66]<-as.factor(january[,66])
january[,67]<-as.factor(january[,67])
january[,69]<-as.factor(january[,69])
january[,70]<-as.factor(january[,70])
january[,71]<-as.factor(january[,71])
january[,72]<-as.factor(january[,72])

str(january)
nrow(january)
prechurn<-which(january[,61]=="1")
length(prechurn)

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=january)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=january)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(january))
selejan<-january[,c(70,5,71,10,4,52,72,1,44,66,67,24,61)]
names(selejan)

targetVar<-"one_month_churn"
xVars<-names(selejan[,-13])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))


trainweight<-as.vector(rep(NA,4652))
trainchurn<-which(train_jan[,13]=="1")
trainweight[trainchurn]<-10
#trainweight[trainchurn]<-20
trainweight[-trainchurn]<-1
trainweight

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

jan_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_jan)

jan_fitted <- predict(jan_model
                      ,newdata = test_jan[,xVars]
                      # Specifying response means we want the probabilities
                      ,type='response')

jan_pred <- ifelse(jan_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = jan_pred
                             , reference = test_jan[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = jan_pred, trues = test_jan$one_month_churn)

pr <- prediction(jan_pred, test_jan$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



y_jan<-as.factor(train_jan[,targetVar])
x_jan<-train_jan[,xVars]
RF_jan <- randomForest(x = x_jan, y = y_jan
                       , data=train_jan,
                       importance=TRUE,
                       # fit 2000 decision trees!
                       ntree=2000)

varImpPlot(RF_jan)
Prediction_jan <- predict(RF_jan, test_jan, type = "response")
Actual_jan <- test_jan$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)

PRcurve(preds = Prediction_jan, trues = test_jan$one_month_churn)

svm <- svm(modelForm, data=train_jan)
svm.prediction <- predict(svm, test_jan)
confusionMatrix(svm.prediction, test_jan$one_month_churn)



#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


november<-november[,c(-1,-2,-4,-5,-6,-24,-25)]
names(november)
november[,61]<-as.factor(november[,61])
november[,63]<-as.factor(november[,63])
november[,64]<-as.factor(november[,64])
november[,65]<-as.factor(november[,65])
november[,66]<-as.factor(november[,66])
november[,67]<-as.factor(november[,67])
november[,69]<-as.factor(november[,69])
november[,70]<-as.factor(november[,70])
november[,71]<-as.factor(november[,71])
november[,72]<-as.factor(november[,72])

str(november)
nrow(november)
prechurn<-which(november[,61]=="1")
length(prechurn)

summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(november))
selenove<-november[,c(70,1,43,4,58,35,55,69,24,12,71,72,57,46,29,61)]


targetVar<-"one_month_churn"
xVars<-names(selenove[,-16])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))


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

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")


balanced_rf <- caret::train(modelForm,
                            data = train_nove,
                            method = "rf",
                            trControl = ctrl)



Prediction_nove <- predict(balanced_rf, test_nove, type = "prob")
nove_pred <- ifelse(Prediction_nove[,2]> 0.5,1,0)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_nove, data = nove_pred)
varImp(balanced_rf)


ctrl <- trainControl(method = "cv", 
                     number = 10,
                     sampling = "up")


balanced_svmweight <- caret::train(modelForm,
                                   data = train_nove,
                                   method = "svmLinearWeights",
                                   trControl = ctrl)

Prediction_nove <- predict(balanced_rf, test_nove)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_nove)


balanced_svmlk <- caret::train(modelForm,
                               data = train_nove,
                               method = "svmLinear2",
                               trControl = ctrl)

Prediction_nove <- predict(balanced_rf, test_nove)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_nove)


balanced_boosting <- caret::train(modelForm,
                                  data = train_nove,
                                  method = "gbm",
                                  trControl = ctrl)

Prediction_nove <- predict(balanced_boosting, test_nove)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_nove, data = Prediction_nove)
varImp(balanced_boosting)


balanced_descent <- caret::train(modelForm,
                                 data = train_nove,
                                 method = "mlpSGD",
                                 trControl = ctrl)

Prediction_nove <- predict(balanced_descent, test_nove)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_nove, data = Prediction_nove)
varImp(balanced_descent)