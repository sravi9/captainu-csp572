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

january<-january[,c(-1,-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67)]
#january<-january[,c(-1,-2,-4,-5,-6,-25,-68)]
names(january)
january[,40]<-as.factor(january[,40])
january[,42]<-as.factor(january[,42])
january[,43]<-as.factor(january[,43])
january[,44]<-as.factor(january[,44])
january[,45]<-as.factor(january[,45])
january[,46]<-as.factor(january[,46])
january[,48]<-as.factor(january[,48])
january[,49]<-as.factor(january[,49])
january[,50]<-as.factor(january[,50])
january[,51]<-as.factor(january[,51])

str(january)
nrow(january)
prechurn<-which(january[,40]=="1")
length(prechurn)

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=january)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=january)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(january))
selejan<-january[,c(49,1,6,50,3,35,51,46,15,40)]
names(selejan)

targetVar<-"one_month_churn"
xVars<-names(selejan[,-10])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))


trainweight<-as.vector(rep(NA,4652))
trainchurn<-which(train_jan[,10]=="1")
trainweight[trainchurn]<-10
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_jan[,10]=="1")

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


november<-november[,c(-1,-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67)]
names(november)
november[,40]<-as.factor(november[,40])
november[,42]<-as.factor(november[,42])
november[,43]<-as.factor(november[,43])
november[,44]<-as.factor(november[,44])
november[,45]<-as.factor(november[,45])
november[,46]<-as.factor(november[,46])
november[,48]<-as.factor(november[,48])
november[,49]<-as.factor(november[,49])
november[,50]<-as.factor(november[,50])
november[,51]<-as.factor(november[,51])

str(november)
nrow(november)
prechurn<-which(november[,40]=="1")
length(prechurn)


nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=november)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=november)

nove_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(november))
selenove<-november[,c(49,1,34,3,26,2,48,15,7,50,5,51,37,20,40)]
names(selenove)

targetVar<-"one_month_churn"
xVars<-names(selenove[,-15])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))

trainchurn<-which(train_nove[,15]=="1")


testchurn<-which(test_nove[,15]=="1")

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


ctrl <- trainControl(method = "cv", 
                     number = 5,
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



balanced_descent <- caret::train(modelForm,
                                  data = train_nove,
                                  method = "mlpSGD",
                                  trControl = ctrl)

Prediction_nove <- predict(balanced_descent, test_nove)
Actual_nove <- test_nove$one_month_churn
confusionMatrix(reference = Actual_nove, data = Prediction_nove)




