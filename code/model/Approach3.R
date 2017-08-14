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


trainweight<-as.vector(rep(NA,nrow(train_jan)))
trainchurn<-which(train_jan[,6]=="1")
trainweight[trainchurn]<-15
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_jan[,6]=="1")

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

targetVar<-"one_month_churn"
xVars<-names(delta_jan[,-40])
inTrain <- createDataPartition(y = delta_jan[,targetVar], list = FALSE, p = .8)
train_jan <- delta_jan[inTrain,]
test_jan <- delta_jan[-inTrain,]
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



nvmodel <- naiveBayes(modelForm, data = train_jan)
class(nvmodel)
summary(nvmodel)
probs <- predict(nvmodel, newdata = test_jan, type = 'raw')
preds <- predict(nvmodel, newdata = test_jan, type = 'class')
preds <- (probs[,'0'] <= probs[,'1'])*1
summary(preds)
conf_matrix <- table(preds, test_jan$one_month_churn)
confusionMatrix(conf_matrix)






#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


octoid<-october[,1]
sepid<-september[,1]
later<-match(sepid,octoid)
cancle<-which(is.na(later)==TRUE)
sep_temp<-september[-cancle,]
sepid<-sep_temp[,1]
later<-match(sepid,octoid)
october<-october[later,]

delta_octo<-sep_temp-october
delta_octo<-delta_octo[,-1]
delta_octo[,40]<-october[,41]

names(delta_octo)
delta_octo[,40]<-as.factor(delta_octo[,40])

delta_octo[,-40]<-scale(delta_octo[,-40])
delta_octo[is.na(delta_octo)]<-0

str(delta_octo)
nrow(delta_octo)
prechurn<-which(delta_octo[,40]=="1")
length(prechurn)


nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=delta_octo)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=delta_octo)

octo_selection <- stepAIC(fullModel,direction="both")
summary(octo_selection)
octo_variable <- rownames(summary(octo_selection)$coefficients)[-1]

match(octo_variable,names(delta_octo))
seleocto<-delta_octo[,c(8,9,12,33,40)]
names(seleocto)

targetVar<-"one_month_churn"
xVars<-names(seleocto[,-5])
set.seed(600)
inTrain <- createDataPartition(y = seleocto[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto[inTrain,]
test_octo <- seleocto[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto))


trainweight<-as.vector(rep(NA,nrow(train_octo)))
trainchurn<-which(train_octo[,5]=="1")
trainweight[trainchurn]<-20
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(test_octo[,5]=="1")

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




seleocto2<-october[,c(49,1,6,50,3,35,51,46,15,40)]
targetVar<-"one_month_churn"
xVars<-names(seleocto2[,-10])
set.seed(600)
inTrain <- createDataPartition(y = seleocto2[,targetVar], list = FALSE, p = .8)
train_octo <- seleocto2[inTrain,]
test_octo <- seleocto2[-inTrain,]
stopifnot(nrow(train_octo) + nrow(test_octo) == nrow(seleocto2))


trainweight<-as.vector(rep(NA,11109))
trainchurn<-which(train_octo[,10]=="1")
trainweight[trainchurn]<-20
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
