library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
library(MASS)
library(caret)
library(ROCR)
library(e1071)
library(logistf)
library(lars)
library(unbalanced)
analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
january<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
september<-analysisdata[which(analysisdata[,"Timetag"]=="2015/9/1"),]
october<-analysisdata[which(analysisdata[,"Timetag"]=="2015/10/1"),]
november<-analysisdata[which(analysisdata[,"Timetag"]=="2015/11/1"),]
december<-analysisdata[which(analysisdata[,"Timetag"]=="2015/12/1"),]

january<-january[,c(-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-69,-70,-71,-72,-73,-74,-75,-76,-78,-79)]
september<-september[,c(-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-69,-70,-71,-72,-73,-74,-75,-76,-78,-79)]
october<-october[,c(-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-69,-70,-71,-72,-73,-74,-75,-76,-78,-79)]
november<-november[,c(-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-69,-70,-71,-72,-73,-74,-75,-76,-78,-79)]
december<-december[,c(-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-69,-70,-71,-72,-73,-74,-75,-76,-78,-79)]


janid<-january[,1]
deceid<-december[,1]
later<-match(deceid,janid)
cancle<-which(is.na(later)==TRUE)
december_temp<-december[-cancle,]
deceid<-december_temp[,1]
later<-match(deceid,janid)
january<-january[later,]

delta_jan<-december_temp-january
delta_jan<-delta_jan[,-1]
delta_jan[,40]<-january[,41]

names(delta_jan)
delta_jan[,40]<-as.factor(delta_jan[,40])

delta_jan[,-40]<-scale(delta_jan[,-40])
delta_jan[is.na(delta_jan)]<-0

str(delta_jan)
nrow(delta_jan)
prechurn<-which(delta_jan[,40]=="1")
length(prechurn)

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=delta_jan)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=delta_jan)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(delta_jan))
selejan<-delta_jan[,c(16,31,35,20,10,40)]
names(selejan)


targetVar<-"one_month_churn"
xVars<-names(selejan[,-6])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))

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
                     repeats = 5, 
                     verboseIter = FALSE,
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



noveid<-november[,1]
octoid<-october[,1]
later<-match(octoid,noveid)
cancle<-which(is.na(later)==TRUE)
octo_temp<-october[-cancle,]
octoid<-octo_temp[,1]
later<-match(octoid,noveid)
november<-november[later,]

delta_nove<-octo_temp-november
delta_nove<-delta_nove[,-1]
delta_nove[,40]<-november[,41]

names(delta_nove)
delta_nove[,40]<-as.factor(delta_nove[,40])

delta_nove[,-40]<-scale(delta_nove[,-40])
delta_nove[is.na(delta_nove)]<-0

str(delta_nove)
nrow(delta_nove)
prechurn<-which(delta_nove[,40]=="1")
length(prechurn)


nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=delta_nove)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=delta_nove)

nove_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(delta_nove))
selenove<-delta_nove[,c(35,15,28,7,17,40)]
names(selenove)

targetVar<-"one_month_churn"
xVars<-names(selenove[,-6])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))


trainweight<-as.vector(rep(NA,nrow(train_nove)))
trainchurn<-which(train_nove[,6]=="1")
trainweight[trainchurn]<-22
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_nove[,6]=="1")

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





selenove2<-november[,c(49,1,6,50,3,35,51,46,15,40)]
targetVar<-"one_month_churn"
xVars<-names(selenove2[,-10])
set.seed(600)
inTrain <- createDataPartition(y = selenove2[,targetVar], list = FALSE, p = .8)
train_nove <- selenove2[inTrain,]
test_nove <- selenove2[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove2))


trainweight<-as.vector(rep(NA,11012))
trainchurn<-which(train_nove[,10]=="1")
trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_nove[,10]=="1")

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


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################



deceid<-december[,1]
noveid<-november[,1]
later<-match(noveid,deceid)
cancle<-which(is.na(later)==TRUE)
nove_temp<-november[-cancle,]
noveid<-nove_temp[,1]
later<-match(noveid,deceid)
december<-december[later,]

delta_dece<-nove_temp-december
delta_dece<-delta_dece[,-1]
delta_dece[,40]<-december[,41]

names(delta_dece)
delta_dece[,40]<-as.factor(delta_dece[,40])

delta_dece[,-40]<-scale(delta_dece[,-40])
delta_dece[is.na(delta_dece)]<-0

str(delta_dece)
nrow(delta_dece)
prechurn<-which(delta_dece[,40]=="1")
length(prechurn)

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=delta_dece)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=delta_dece)

dece_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(dece_Selection)
dece_variable <- rownames(summary(dece_Selection)$coefficients)[-1]

match(dece_variable,names(delta_dece))
seledece<-delta_dece[,c(9,35,37,31,40)]
names(seledece)

targetVar<-"one_month_churn"
xVars<-names(seledece[,-5])
set.seed(600)
inTrain <- createDataPartition(y = seledece[,targetVar], list = FALSE, p = .8)
train_dece <- seledece[inTrain,]
test_dece <- seledece[-inTrain,]
stopifnot(nrow(train_dece) + nrow(test_dece) == nrow(seledece))


trainweight<-as.vector(rep(NA,nrow(train_dece)))
trainchurn<-which(train_dece[,5]=="1")
trainweight[trainchurn]<-18
#trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_dece[,5]=="1")

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







seledece2<-december[,c(49,1,6,50,3,35,51,46,15,40)]
names(seledece2)

targetVar<-"one_month_churn"
xVars<-names(seledece2[,-10])
set.seed(600)
inTrain <- createDataPartition(y = seledece2[,targetVar], list = FALSE, p = .8)
train_dece <- seledece2[inTrain,]
test_dece <- seledece2[-inTrain,]
stopifnot(nrow(train_dece) + nrow(test_dece) == nrow(seledece2))


trainweight<-as.vector(rep(NA,10327))
trainchurn<-which(train_dece[,10]=="1")
trainweight[trainchurn]<-15
#trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_dece[,10]=="1")

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
