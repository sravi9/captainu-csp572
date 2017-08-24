library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
library(MASS)
library(caret)
library(ROCR)
library(e1071)
library(logistf)
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

january[persist,61]<-0
january[-persist,61]<-1
january[churnadd,61]<-1
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

january[persist,61]<-0
january[-persist,61]<-1
january[churnadd,61]<-1
names(january)[61]<-"three_month_churn"

str(january)
nrow(january)
prechurn<-which(january[,61]=="1")
length(prechurn)

selejan<-january[,c(70,71,48,4,59,15,18,17,8,49,12,62,40,29,52,46,61)]
names(selejan)

targetVar<-"three_month_churn"
xVars<-names(selejan[,-17])
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

november[persist,61]<-0
november[-persist,61]<-1
november[churnadd,61]<-1
names(november)[61]<-"three_month_churn"

str(november)
nrow(november)
prechurn<-which(november[,61]=="1")
length(prechurn)

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=november)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=november)

nove_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(nove_Selection)
nove_variable <- rownames(summary(nove_Selection)$coefficients)[-1]

match(nove_variable,names(november))
selenove<-november[,c(70,1,68,8,35,21,15,31,44,37,40,24,9,4,12,69,38,19,65,67,39,61)]


targetVar<-"three_month_churn"
xVars<-names(selenove[,-22])
set.seed(600)
inTrain <- createDataPartition(y = selenove[,targetVar], list = FALSE, p = .8)
train_nove <- selenove[inTrain,]
test_nove <- selenove[-inTrain,]
stopifnot(nrow(train_nove) + nrow(test_nove) == nrow(selenove))


trainweight<-as.vector(rep(NA,nrow(train_nove)))
trainchurn<-which(train_nove[,22]=="1")
trainweight[trainchurn]<-2
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_nove[,22]=="1")

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

