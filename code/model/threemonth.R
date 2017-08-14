library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
library(MASS)
library(caret)
library(ROCR)
analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
january<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
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

str(january)
nrow(january)
prechurn<-which(january[,61]=="1")
length(prechurn)

nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,data=january)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,data=january)

autoSelection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(autoSelection)
variableSelected <- rownames(summary(autoSelection)$coefficients)[-1]


nullModel <- glm(formula = one_month_churn ~ 1,family=binomial,weights = weightvec, data=january)
fullModel <- glm(formula = one_month_churn ~ .,family=binomial,weights = weightvec, data=january)

autoSelection <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")
summary(autoSelection)
variableSelected <- rownames(summary(autoSelection)$coefficients)[-1]
modelselect<-stepAIC(fullModel,direction="both")
variablechosen<-rownames(summary(modelselect)$coefficients)[-1]


setdiff(variableSelected,variablechosen)
match(variableSelected,names(january))
selecteddata<-january[,c(1,2,3,6,10,11,12,13,14,15,17,18,19,21,24,26,27,29,35,38,
                          41,42,43,44,49,50,52,56,57,58,60,63,66,67,68,71,72,61)]

names(january)
glmdata<-january[,c(48,4,44,59,18,8,12,49,46,50,2,70,71,62,61)]


targetVar<-"one_month_churn"
xVars<-names(glmdata[,-15])
set.seed(100)
inTrain <- createDataPartition(y = glmdata[,targetVar], list = FALSE, p = .8)
traindata <- glmdata[inTrain,]
testdata <- glmdata[-inTrain,]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(glmdata))


createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

testmodel <- glm(modelForm,family=binomial(link='logit'), data=traindata)

testfitted <- predict(testmodel
                      ,newdata = testdata[,xVars]
                      # Specifying response means we want the probabilities
                      ,type='response')

testpred <- ifelse(testfitted > 0.5,1,0)
confusion <- confusionMatrix(data = testpred
                             , reference = testdata[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion






targetVar<-"one_month_churn"
xVars<-names(january[,-61])
set.seed(600)
inTrain <- createDataPartition(y = january[,targetVar], list = FALSE, p = .8)
traindata <- january[inTrain,]
testdata <- january[-inTrain,]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(january))


trainweight<-as.vector(rep(NA,4652))
trainchurn<-which(traindata[,38]=="1")
trainweight[trainchurn]<-20
trainweight[-trainchurn]<-1
trainweight

testchurn<-which(testdata[,38]=="1")

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)

testmodel <- glm(modelForm,family=binomial(link='logit'),weights = trainweight, data=traindata)

testfitted <- predict(testmodel
                      ,newdata = testdata[,xVars]
                      # Specifying response means we want the probabilities
                      ,type='response')

testpred <- ifelse(testfitted > 0.5,1,0)
confusion <- confusionMatrix(data = testpred
                             , reference = testdata[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion

PRcurve(preds = testpred, trues = testdata$one_month_churn)

pr <- prediction(testpred, testdata$one_month_churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc







y<-as.factor(traindata[,targetVar])
x<-traindata[,xVars]
fit4 <- randomForest(x = x, y = y
                     , data=traindata,
                     importance=TRUE,
                     # fit 2000 decision trees!
                     ntree=500)

varImpPlot(fit4)
Prediction4 <- predict(fit4, testdata, type = "response")
Actual <- testdata$one_month_churn
confusionMatrix(reference = Actual, data = Prediction4)

PRcurve(preds = Prediction4, trues = testdata$onemonthchurn)

svm <- svm(modelForm, data=traindata)
svm.prediction <- predict(svm, testdata)
confusionMatrix(svm.prediction, testdata$one_month_churn)




createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)
reducedmodel <- glm(modelForm,family=binomial(link='logit'),data=traindata)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvtestmd<-cv.glm(testdata,reducedmodel,cost,K=10)


library(e1071)
svmmodel<-svm(modelForm,traindata, probability = TRUE)
svmfitted <- predict(svmmodel, testdata,type = "response")
svmpred <- ifelse(svmfitted > 0.5,1,0)
confusion <- confusionMatrix(data = svmpred
                             , reference = testdata[,targetVar]
                             , dnn = c("Predicted Surival", 'Actual Survival')
)
confusion
svmpred


library(lars)
laa<-lars(x,y)


library(glmnet)
x<-as.matrix(x)
y<-as.matrix(y)
cvfit = cv.glmnet(x = x, y = y, family = "binomial", weights = trainweight, type.measure = "auc")



