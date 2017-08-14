library(leaps)
library(MASS)
library('DMwR')
library(randomForest)
analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
premonth<-analysisdata[which(analysisdata[,"Timetag"]=="2016/1/1"),]
prechurn<-which(premonth[,74]=="1")
premonth<-premonth[-prechurn,]
latermonth<-analysisdata[which(analysisdata[,"Timetag"]=="2016/2/1"),]
preid<-premonth[,1]
laterid<-latermonth[,1]
later<-match(preid,laterid)
cancle<-which(is.na(later)==TRUE)
premonth<-premonth[-cancle,]
preid<-premonth[,1]
later<-match(preid,laterid)
latermonth<-latermonth[later,]
premonth[,56]<-latermonth[,56]
names(premonth)[56]<-"onemonthchurn"

premonth<-premonth[,c(-1,-2,-4,-5,-6,-8,-10,-12,-14,-16,-18,-20,-21,-45,-47,-49,-51,-53,-55)]
names(premonth)
premonth[,51]<-as.factor(premonth[,51])
premonth[,52]<-as.factor(premonth[,52])

library(caret)
targetVar<-"onemonthchurn"
xVars<-names(premonth[,-37])
set.seed(100)
inTrain <- createDataPartition(y = premonth[,targetVar], list = FALSE, p = .8)
traindata <- premonth[inTrain,]
testdata <- premonth[-inTrain,]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(premonth))


createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

modelForm <- createModelFormula(targetVar, xVars)
library(boot)
testmodel <- glm(modelForm,family=binomial(link='logit'),data=traindata)

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
PRcurve(preds = testpred, trues = testdata$onemonthchurn)


modelselect<-stepAIC(testmodel,direction="both")
selectfitted <- predict(modelselect
                      ,newdata = testdata[,xVars]
                      ,type='response')

selectpred <- ifelse(selectfitted > 0.5,1,0)
confusion <- confusionMatrix(data = selectpred
                             , reference = testdata[,targetVar]
                             , dnn = c("Predicted Churn", 'Actual Churn')
)
confusion
PRcurve(preds = selectpred, trues = testdata$onemonthchurn)

premonth<-premonth[,c(-1,-2,-3,-4,-5,-6,-7,-9,-10,-12,-13,-14,-15,-17,-18,-20,-21,-22,-23,-24,
                      -26,-27,-28,-29,-30,-32,-33,-36,-38,-39,-40,-41,-42,-43,-44,-45,-46,-47,-49,-50,-51-54)]
xVars<-names(premonth[,-9])
set.seed(100)
inTrain <- createDataPartition(y = premonth[,targetVar], list = FALSE, p = .8)
traindata <- premonth[inTrain,]
testdata <- premonth[-inTrain,]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(premonth))



y<-as.factor(traindata[,targetVar])
x<-traindata[,xVars]
fit4 <- randomForest(x = x, y = y
                     , data=traindata,
                     importance=TRUE,
                     # fit 2000 decision trees!
                     ntree=500)

varImpPlot(fit4)
Prediction4 <- predict(fit4, testdata, type = "response")
Actual <- testdata$onemonthchurn
confusionMatrix(reference = Actual, data = Prediction4)

PRcurve(preds = Prediction4, trues = testdata$onemonthchurn)


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
