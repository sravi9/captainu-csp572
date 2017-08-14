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

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=january)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=january)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(january))
selejan<-january[,c(49,50,39,3,9,35,5,7,31,20,37,41,40)]
names(selejan)

targetVar<-"three_month_churn"
xVars<-names(selejan[,-13])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))


trainweight<-as.vector(rep(NA,nrow(train_jan)))
trainchurn<-which(train_jan[,13]=="1")
trainweight[trainchurn]<-2
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

jan_model <- glm(modelForm,family=binomial(link='logit'), data=train_jan)

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

PRcurve(preds = jan_pred, trues = test_jan$three_month_churn)

pr <- prediction(jan_pred, test_jan$three_month_churn)
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
Actual_jan <- test_jan$three_month_churn
confusionMatrix(reference = Actual_jan, data = Prediction_jan)

PRcurve(preds = Prediction_jan, trues = test_jan$three_month_churn)

svm <- svm(modelForm, data=train_jan)
svm.prediction <- predict(svm, test_jan)
confusionMatrix(svm.prediction, test_jan$three_month_churn)


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

oid<-october[,1]
did<-december[,1]
persist<-match(did,oid)
persist<-na.omit(persist)
later<-match(oid,did)
later<-na.omit(later)
dchurn<-which(december[later,68]==1)
dchurnid<-march[dchurn,1]
churnadd<-match(dchurnid,oid)
churnadd<-na.omit(churnadd)

october<-october[,c(-1,-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67)]
#october<-october[,c(-1,-2,-4,-5,-6,-25,-68)]
names(october)
october[,40]<-as.factor(october[,40])
october[,42]<-as.factor(october[,42])
october[,43]<-as.factor(october[,43])
october[,44]<-as.factor(october[,44])
october[,45]<-as.factor(october[,45])
october[,46]<-as.factor(october[,46])
october[,48]<-as.factor(october[,48])
october[,49]<-as.factor(october[,49])
october[,50]<-as.factor(october[,50])
october[,51]<-as.factor(october[,51])

october[persist,40]<-0
october[-persist,40]<-1
october[churnadd,40]<-1
names(october)[40]<-"three_month_churn"

str(october)
nrow(october)
prechurn<-which(october[,40]=="1")
length(prechurn)


nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=october)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=october)

octo_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(octo_selection)
octo_variable <- rownames(summary(octo_selection)$coefficients)[-1]

match(octo_variable,names(october))
seleocto<-october[,c(49,47,34,1,51,43,44,50,41,26,37,21,11,20,2,40)]
names(seleocto)

targetVar<-"three_month_churn"
xVars<-names(seleocto[,-16])
set.seed(550)
inTrain <- createDataPartition(y = seleocto[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto[inTrain,]
test_octo <- seleocto[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto))


trainweight<-as.vector(rep(NA,nrow(train_octo)))
trainchurn<-which(train_octo[,16]=="1")
trainweight[trainchurn]<-3
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,16]=="1")

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

PRcurve(preds = octo_pred, trues = test_octo$three_month_churn)

pr <- prediction(octo_pred, test_octo$three_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc




seleocto2<-october[,c(49,50,39,3,9,35,5,7,31,20,37,41,40)]
targetVar<-"three_month_churn"
xVars<-names(seleocto2[,-13])
set.seed(600)
inTrain <- createDataPartition(y = seleocto2[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto2[inTrain,]
test_octo <- seleocto2[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto2))


trainweight<-as.vector(rep(NA,nrow(train_octo)))
trainchurn<-which(train_octo[,13]=="1")
trainweight[trainchurn]<-3
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,10]=="1")

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


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

december<-analysisdata[which(analysisdata[,"Timetag"]=="2015/12/1"),]
did<-december[,1]
fid<-february[,1]
persist<-match(fid,did)
persist<-na.omit(persist)
later<-match(did,fid)
later<-na.omit(later)
fchurn<-which(february[later,68]==1)
fchurnid<-march[fchurn,1]
churnadd<-match(fchurnid,did)
churnadd<-na.omit(churnadd)

december<-december[,c(-1,-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-22,-24,-25,-56,-57,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67)]
names(december)
december[,40]<-as.factor(december[,40])
december[,42]<-as.factor(december[,42])
december[,43]<-as.factor(december[,43])
december[,44]<-as.factor(december[,44])
december[,45]<-as.factor(december[,45])
december[,46]<-as.factor(december[,46])
december[,48]<-as.factor(december[,48])
december[,49]<-as.factor(december[,49])
december[,50]<-as.factor(december[,50])
december[,51]<-as.factor(december[,51])

december[persist,40]<-0
december[-persist,40]<-1
december[churnadd,40]<-1
names(december)[40]<-"three_month_churn"

str(december)
nrow(december)
prechurn<-which(december[,40]=="1")
length(prechurn)

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=december)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=december)

dece_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(dece_Selection)
dece_variable <- rownames(summary(dece_Selection)$coefficients)[-1]

match(dece_variable,names(december))
seledece<-december[,c(49,39,47,1,41,3,7,43,11,51,31,2,10,15,32,40)]
names(seledece)

targetVar<-"three_month_churn"
xVars<-names(seledece[,-16])
set.seed(600)
inTrain <- createDataPartition(y = seledece[,targetVar], list = FALSE, p = .8)
train_dece <- seledece[inTrain,]
test_dece <- seledece[-inTrain,]
stopifnot(nrow(train_dece) + nrow(test_dece) == nrow(seledece))


trainweight<-as.vector(rep(NA,10327))
trainchurn<-which(train_dece[,16]=="1")
trainweight[trainchurn]<-2
#trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_dece[,16]=="1")

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

PRcurve(preds = dece_pred, trues = test_dece$three_month_churn)

pr <- prediction(dece_pred, test_dece$three_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc







seledece2<-december[,c(49,1,6,50,3,35,51,46,15,40)]
names(seledece2)

targetVar<-"three_month_churn"
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
