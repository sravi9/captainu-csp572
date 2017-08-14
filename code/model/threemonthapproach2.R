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
names(january)[61]<-"three_month_churn"

str(january)
nrow(january)
prechurn<-which(january[,61]=="1")
length(prechurn)

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=january)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=january)

jan_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(jan_selection)
jan_variable <- rownames(summary(jan_selection)$coefficients)[-1]

match(jan_variable,names(january))
selejan<-january[,c(70,71,48,4,59,15,18,17,8,49,12,62,40,29,52,46,61)]
names(selejan)

targetVar<-"three_month_churn"
xVars<-names(selejan[,-17])
set.seed(600)
inTrain <- createDataPartition(y = selejan[,targetVar], list = FALSE, p = .8)
train_jan <- selejan[inTrain,]
test_jan <- selejan[-inTrain,]
stopifnot(nrow(train_jan) + nrow(test_jan) == nrow(selejan))


trainweight<-as.vector(rep(NA,4652))
trainchurn<-which(train_jan[,17]=="1")
trainweight[trainchurn]<-2
#trainweight[trainchurn]<-20
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_jan[,17]=="1")

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

ltf<-logistf(modelForm,data = train_jan)
betas <- coef(ltf)
X <- test_jan[,xVars]      
pi.obs <- 1 / (1 + exp(-X %*% betas))



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

october<-october[,c(-1,-2,-4,-5,-6,-24,-25)]
#october<-october[,c(-1,-2,-4,-5,-6,-25,-68)]
names(october)
october[,61]<-as.factor(october[,61])
october[,63]<-as.factor(october[,63])
october[,64]<-as.factor(october[,64])
october[,65]<-as.factor(october[,65])
october[,66]<-as.factor(october[,66])
october[,67]<-as.factor(october[,67])
october[,69]<-as.factor(october[,69])
october[,70]<-as.factor(october[,70])
october[,71]<-as.factor(october[,71])
october[,72]<-as.factor(october[,72])

october[persist,61]<-0
october[-persist,61]<-1
october[churnadd,61]<-1
names(october)[61]<-"three_month_churn"

str(october)
nrow(october)
prechurn<-which(october[,61]=="1")
length(prechurn)

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=october)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=october)

octo_selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(octo_selection)
octo_variable <- rownames(summary(octo_selection)$coefficients)[-1]

match(octo_variable,names(october))
match(octo_variable,jan_variable)
seleocto<-october[,c(70,68,43,1,72,64,58,62,65,71,30,2,49,50,46,59,20,11,41,61)]


targetVar<-"three_month_churn"
xVars<-names(seleocto[,-20])
set.seed(600)
inTrain <- createDataPartition(y = seleocto[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto[inTrain,]
test_octo <- seleocto[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto))


trainweight<-as.vector(rep(NA,nrow(train_octo)))
trainchurn<-which(train_octo[,20]=="1")
trainweight[trainchurn]<-2
#trainweight[trainchurn]<-10
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,20]=="1")

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




seleocto2<-october[,c(70,5,71,10,4,52,72,1,44,66,67,24,61)]
targetVar<-"three_month_churn"
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

y_octo<-as.factor(train_octo[,targetVar])
x_octo<-train_octo[,xVars]
RF_octo <- randomForest(x = x_octo, y = y_octo
                       , data=train_octo,
                       importance=TRUE,
                       # fit 2000 decision trees!
                       ntree=2000)

varImpPlot(RF_octo)
Prediction_octo <- predict(RF_octo, test_octo, type = "response")
Actual_octo <- test_octo$three_month_churn
confusionMatrix(reference = Actual_octo, data = Prediction_octo)




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

december<-december[,c(-1,-2,-4,-5,-6,-24,-25)]
names(december)
december[,61]<-as.factor(december[,61])
december[,63]<-as.factor(december[,63])
december[,64]<-as.factor(december[,64])
december[,65]<-as.factor(december[,65])
december[,66]<-as.factor(december[,66])
december[,67]<-as.factor(december[,67])
december[,69]<-as.factor(december[,69])
december[,70]<-as.factor(december[,70])
december[,71]<-as.factor(december[,71])
december[,72]<-as.factor(december[,72])

december[persist,61]<-0
december[-persist,61]<-1
december[churnadd,61]<-1
names(december)[61]<-"three_month_churn"

str(december)
nrow(december)
prechurn<-which(december[,61]=="1")
length(prechurn)

nullModel <- glm(formula = three_month_churn ~ 1,family=binomial,data=december)
fullModel <- glm(formula = three_month_churn ~ .,family=binomial,data=december)

dece_Selection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(dece_Selection)
dece_variable <- rownames(summary(dece_Selection)$coefficients)[-1]

match(dece_variable,names(december))
seledece<-december[,c(70,48,68,1,62,4,12,64,20,72,40,2,19,24,41,49,50,15,16,61)]


targetVar<-"three_month_churn"
xVars<-names(seledece[,-20])
set.seed(600)
inTrain <- createDataPartition(y = seledece[,targetVar], list = FALSE, p = .8)
train_dece <- seledece[inTrain,]
test_dece <- seledece[-inTrain,]
stopifnot(nrow(train_dece) + nrow(test_dece) == nrow(seledece))


trainweight<-as.vector(rep(NA,nrow(train_dece)))
trainchurn<-which(train_dece[,20]=="1")
trainweight[trainchurn]<-2
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_dece[,20]=="1")

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
