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
february<-analysisdata[which(analysisdata[,"Timetag"]=="2016/2/1"),]
march<-analysisdata[which(analysisdata[,"Timetag"]=="2016/3/1"),]


jid<-january[,1]
mid<-march[,1]
persist<-match(mid,jid)
persist<-na.omit(persist)
later<-match(jid,mid)
later<-na.omit(later)
mchurn<-which(march[later,68]==1)
mchurnid<-march[mchurn,1]
churnadd<-match(mchurnid,jid)
churnadd<-na.omit(churnadd)

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

january[persist,40]<-0
january[-persist,40]<-1
january[churnadd,40]<-1
names(january)[40]<-"three_month_churn"

str(january)
nrow(january)
prechurn<-which(january[,40]=="1")
length(prechurn)

selejan<-january[,c(49,50,39,3,9,35,5,7,31,20,37,41,40)]
names(selejan)

targetVar<-"three_month_churn"
xVars<-names(selejan[,-13])
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

ctrl <- trainControl(method = "cv", 
                     number = 10,
                     sampling = "up")

balanced_rf <- caret::train(modelForm,
                            data = train_jan,
                            method = "rf",
                            trControl = ctrl)

Prediction_jan <- predict(balanced_rf, test_jan, type = "prob")
jan_pred <- ifelse(Prediction_jan[,2]> 0.5,1,0)
Actual_jan <- test_jan$three_month_churn
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
Actual_jan <- test_jan$three_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)
varImp(balanced_boosting)


balanced_descent <- caret::train(modelForm,
                                 data = train_jan,
                                 method = "mlpSGD",
                                 trControl = ctrl)

Prediction_jan <- predict(balanced_boosting, test_jan)
Actual_jan <- test_jan$three_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)
varImp(balanced_descent)


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################



january<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
nid<-november[,1]
jid<-january[,1]
persist<-match(jid,nid)
persist<-na.omit(persist)
later<-match(nid,jid)
later<-na.omit(later)
jchurn<-which(january[later,68]==1)
jchurnid<-march[jchurn,1]
churnadd<-match(jchurnid,nid)
churnadd<-na.omit(churnadd)


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

november[persist,40]<-0
november[-persist,40]<-1
november[churnadd,40]<-1
names(november)[40]<-"three_month_churn"

str(november)
nrow(november)
prechurn<-which(november[,40]=="1")
length(prechurn)


nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=november)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=november)

nove_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(november))
selenove<-november[,c(49,1,47,5,26,12,22,28,31,15,34,7,44,3,29,10,48,46,30,40)]
names(selenove)

targetVar<-"three_month_churn"
xVars<-names(selenove[,-20])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))


trainweight<-as.vector(rep(NA,11012))
trainchurn<-which(train_nove[,20]=="1")
trainweight[trainchurn]<-2
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_nove[,20]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

nove_model <- glm(modelForm,family=binomial(link='logit'), data=train_nove)

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

PRcurve(preds = nove_pred, trues = test_nove$three_month_churn)

pr <- prediction(nove_pred, test_nove$three_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc





selenove2<-november[,c(49,1,6,50,3,35,51,46,15,40)]
targetVar<-"three_month_churn"
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

nove_model <- glm(modelForm,family=binomial(link='logit'), data=train_nove)

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


y_nove<-as.factor(train_nove[,targetVar])
x_nove<-train_nove[,xVars]
RF_nov <- randomForest(x = x_nove, y = y_nove
                       , data=train_nove,
                       importance=TRUE,
                       # fit 2000 decision trees!
                       ntree=2000)

varImpPlot(RF_nov)
Prediction_nov <- predict(RF_nov, test_nove, type = "response")
Actual_nov <- test_nove$three_month_churn
confusionMatrix(reference = Actual_nov, data = Prediction_nov)



