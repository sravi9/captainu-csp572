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

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=dummyjanuary)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=dummyjanuary)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(dummyjanuary))
selejan<-dummyjanuary[,c(68,5,72,10,4,52,1,74,67,44,70,24,61)]
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
trainweight[trainchurn]<-15
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


targetVar<-"one_month_churn"
xVars<-names(dummyjanuary[,-61])
set.seed(200)
inTrain <- createDataPartition(y = dummyjanuary[,targetVar], list = FALSE, p = .8)
train_jan <- dummyjanuary[inTrain,]
test_jan <- dummyjanuary[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(dummyjanuary))


x_jan<-train_jan[,xVars]
y_jan<-as.factor(train_jan[,targetVar])
x_jan<-as.matrix(x_jan)
cvfit = cv.glmnet(x = x_jan, y = y_jan, family = "binomial", type.measure = "auc")
testx<-as.matrix(test_jan[,xVars])
response <- predict(cvfit, testx,
                    s = "lambda.min",
                    type = "class")

confusion <- confusionMatrix(data = response
                             , reference = test_jan[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


october<-october[,c(-1,-2,-4,-5,-6,-24,-25,-70,-71,-73,-74,-75,-76)]
#october<-october[,c(-1,-2,-4,-5,-6,-25,-68)]
names(october)
october[,61]<-as.factor(october[,61])
october[,62]<-as.factor(october[,62])
october[,63]<-as.factor(october[,63])
october[,64]<-as.factor(october[,64])
october[,65]<-as.factor(october[,65])
october[,66]<-as.factor(october[,66])


str(october)
nrow(october)
prechurn<-which(october[,61]=="1")
length(prechurn)

dummyoctober<-dummy.data.frame(data = october, names = c("gender","act","Current","Underpromotion","Ralations"))
dummyoctober[,1:60]<-scale(dummyoctober[,1:60])
dummyoctober[is.na(dummyoctober)]<-0

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=dummyoctober)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=dummyoctober)

octo_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(octo_selection)
octo_variable <- rownames(summary(octo_selection)$coefficients)[-1]

match(octo_variable,names(dummyoctober))
seleocto<-dummyoctober[,c(1,68,48,74,72,41,62,70,69,9,12,42,4,30,11,43,46,61)]
names(seleocto)


targetVar<-"one_month_churn"
xVars<-names(seleocto[,-18])
set.seed(600)
inTrain <- createDataPartition(y = seleocto[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto[inTrain,]
test_octo <- seleocto[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto))


trainweight<-as.vector(rep(NA,11109))
trainchurn<-which(train_octo[,18]=="1")
trainweight[trainchurn]<-20
#trainweight[trainchurn]<-10
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,18]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

octo_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_octo)

octo_fitted <- predict(octo_model
                       ,newdata = test_octo[,xVars]
                       # Specifying response means we want the probabilities
                       ,type='response')

octo_pred <- ifelse(octo_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = octo_pred
                             , reference = test_octo[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = octo_pred, trues = test_octo$one_month_churn)

pr <- prediction(octo_pred, test_octo$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc




seleocto2<-dummyoctober[,c(68,5,72,10,4,52,1,74,67,44,70,24,61)]
targetVar<-"one_month_churn"
xVars<-names(seleocto2[,-13])
set.seed(600)
inTrain <- createDataPartition(y = seleocto2[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto2[inTrain,]
test_octo <- seleocto2[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto2))


trainweight<-as.vector(rep(NA,11109))
trainchurn<-which(train_octo[,13]=="1")
trainweight[trainchurn]<-10
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,13]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

octo_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_octo)

octo_fitted <- predict(octo_model
                       ,newdata = test_octo[,xVars]
                       # Specifying response means we want the probabilities
                       ,type='response')

octo_pred <- ifelse(octo_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = octo_pred
                             , reference = test_octo[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion




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


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


december<-december[,c(-1,-2,-4,-5,-6,-24,-25,-70,-71,-73,-74,-75,-76)]
names(december)
december[,61]<-as.factor(december[,61])
december[,62]<-as.factor(december[,62])
december[,63]<-as.factor(december[,63])
december[,64]<-as.factor(december[,64])
december[,65]<-as.factor(december[,65])
december[,66]<-as.factor(december[,66])


str(december)
nrow(december)
prechurn<-which(december[,61]=="1")
length(prechurn)

dummydecember<-dummy.data.frame(data = december, names = c("gender","act","Current","Underpromotion","Ralations"))
dummydecember[,1:60]<-scale(dummydecember[,1:60])
dummydecember[is.na(dummydecember)]<-0

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=dummydecember)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=dummydecember)

dece_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(dece_Selection)
dece_variable <- rownames(summary(dece_Selection)$coefficients)[-1]

match(dece_variable,names(dummydecember))
seledece<-dummydecember[,c(68,48,1,73,59,70,69,62,3,64,50,5,61)]


targetVar<-"one_month_churn"
xVars<-names(seledece[,-13])
set.seed(600)
inTrain <- createDataPartition(y = seledece[,targetVar], list = FALSE, p = .8)
train_dece <- seledece[inTrain,]
test_dece <- seledece[-inTrain,]
stopifnot(nrow(train_dece) + nrow(test_dece) == nrow(seledece))


trainweight<-as.vector(rep(NA,10327))
trainchurn<-which(train_dece[,13]=="1")
trainweight[trainchurn]<-11
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_dece[,13]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

dece_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_dece)

dece_fitted <- predict(dece_model
                       ,newdata = test_dece[,xVars]
                       # Specifying response means we want the probabilities
                       ,type='response')

dece_pred <- ifelse(dece_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = dece_pred
                             , reference = test_dece[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = dece_pred, trues = test_dece$one_month_churn)

pr <- prediction(dece_pred, test_dece$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

march<-march[,c(-1,-2,-4,-5,-6,-24,-25,-70,-71,-73,-74,-75,-76)]
#march<-march[,c(-1,-2,-4,-5,-6,-25,-68)]
names(march)
march[,61]<-as.factor(march[,61])
march[,62]<-as.factor(march[,62])
march[,63]<-as.factor(march[,63])
march[,64]<-as.factor(march[,64])
march[,65]<-as.factor(march[,65])
march[,66]<-as.factor(march[,66])


str(march)
nrow(march)
prechurn<-which(march[,61]=="1")
length(prechurn)

dummymarch<-dummy.data.frame(data = march, names = c("gender","act","Current","Underpromotion","Ralations"))
dummymarch[,1:60]<-scale(dummymarch[,1:60])
dummymarch[is.na(dummymarch)]<-0

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=dummymarch)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=dummymarch)

march_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(march_selection)
march_variable <- rownames(summary(march_selection)$coefficients)[-1]

match(march_variable,names(dummymarch))
selemarch<-dummymarch[,c(69,1,43,72,3,42,12,18,29,62,41,44,17,16,2,61)]
names(selemarch)

targetVar<-"one_month_churn"
xVars<-names(selemarch[,-16])
set.seed(600)
inTrain <- createDataPartition(y = selemarch[,targetVar], list = FALSE, p = .8)
train_march <- selemarch[inTrain,]
test_march <- selemarch[-inTrain,]
stopifnot(nrow(train_march) + nrow(test_march) == nrow(selemarch))


trainweight<-as.vector(rep(NA,nrow(train_march)))
trainchurn<-which(train_march[,16]=="1")
trainweight[trainchurn]<-10
#trainweight[trainchurn]<-20
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_march[,16]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

march_model <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=train_march)

march_fitted <- predict(march_model
                      ,newdata = test_march[,xVars]
                      # Specifying response means we want the probabilities
                      ,type='response')

march_pred <- ifelse(jan_fitted > 0.5,1,0)
confusion <- confusionMatrix(data = march_pred
                             , reference = test_march[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = jan_pred, trues = test_march$one_month_churn)

pr <- prediction(jan_pred, test_march$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



y_jan<-as.factor(train_march[,targetVar])
x_jan<-train_march[,xVars]
RF_jan <- randomForest(x = x_jan, y = y_jan
                       , data=train_march,
                       importance=TRUE,
                       # fit 2000 decision trees!
                       ntree=2000)

varImpPlot(RF_jan)
Prediction_jan <- predict(RF_jan, test_march, type = "response")
Actual_jan <- test_march$one_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)

PRcurve(preds = Prediction_jan, trues = test_march$one_month_churn)

svm <- svm(modelForm, data=train_march)
svm.prediction <- predict(svm, test_march)
confusionMatrix(svm.prediction, test_march$one_month_churn)


targetVar<-"one_month_churn"
xVars<-names(dummymarch[,-61])
set.seed(200)
inTrain <- createDataPartition(y = dummymarch[,targetVar], list = FALSE, p = .8)
train_march <- dummymarch[inTrain,]
test_march <- dummymarch[-inTrain,]
stopifnot(nrow(train_march) + nrow(test_march) == nrow(dummymarch))


x_jan<-train_march[,xVars]
y_jan<-as.factor(train_march[,targetVar])
x_jan<-as.matrix(x_jan)
cvfit = cv.glmnet(x = x_jan, y = y_jan, family = "binomial", weights = trainweight, type.measure = "auc")
testx<-as.matrix(test_march[,xVars])
response <- predict(cvfit, testx,
                    s = "lambda.min",
                    type = "class")

confusion <- confusionMatrix(data = response
                             , reference = test_march[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion




names(dummymarch)


haha<-dummymarch[,c(1,2,4,10,12,14,21,24,25,26,27,28,29,31,35,41,42,43,44,46,47,48,61,62,63)]
targetVar<-"one_month_churn"
xVars<-names(haha[,-23])
set.seed(200)
inTrain <- createDataPartition(y = haha[,targetVar], list = FALSE, p = .8)
train_march <- haha[inTrain,]
test_march <- haha[-inTrain,]
stopifnot(nrow(train_march) + nrow(test_march) == nrow(haha))


x_jan<-train_march[,xVars]
y_jan<-as.factor(train_march[,targetVar])
x_jan<-as.matrix(x_jan)
cvfit = cv.glmnet(x = x_jan, y = y_jan, family = "binomial", weights = trainweight, type.measure = "auc")
testx<-as.matrix(test_march[,xVars])
response <- predict(cvfit, testx,
                    s = "lambda.min",
                    type = "class")

confusion <- confusionMatrix(data = response
                             , reference = test_march[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion
