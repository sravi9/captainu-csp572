analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
churn<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/churn.csv', header = TRUE)
summary(churn[,"Timetag"])
analysisdatatest<-analysisdata
analysisdatatest<-analysisdatatest[-which(analysisdatatest[,"Timetag"]=="2012/1/1"),]
analysisdatatest<-analysisdatatest[-which(analysisdatatest[,"Timetag"]=="2012/2/1"),]
analysisdatatest<-analysisdatatest[-which(analysisdatatest[,"Timetag"]=="2012/3/1"),]
analysisdatatest<-analysisdatatest[-which(analysisdatatest[,"Timetag"]=="2012/4/1"),]
analysisdatatest<-analysisdatatest[-which(analysisdatatest[,"Timetag"]=="2012/5/1"),]
analysisdatatest<-analysisdatatest[,c(-1,-2,-3,-5,-6,-20)]
analysisdatatest[,46]<-as.factor(analysisdatatest[,46])
analysisdatatest[,47]<-as.factor(analysisdatatest[,47])
str(analysisdatatest)
library(caret)
targetVar<-"Churn"
xVars<-names(analysisdatatest[,-15])
inTrain <- createDataPartition(y = analysisdatatest[,targetVar], list = FALSE, p = .8)
traindata <- analysisdatatest[inTrain,]
testdata <- analysisdatatest[-inTrain,]
testdata<-testdata[,-15]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(analysisdatatest))


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
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cvtestmd<-cv.glm(testdata,testmodel,cost,K=10)

library(randomForest)
x = traindata[,xVars]
y = as.factor(traindata[, targetVar])
fit4 <- randomForest(x = x, y = y
                     , data=traindata,
                     importance=TRUE,
                     # fit 2000 decision trees!
                     ntree=50)
str(traindata)
names(traindata)
traindata<-traindata[,-38]
xVar<-names(traindata[,-15])
x = traindata[,xVar]
y = as.factor(traindata[, targetVar])
fit4 <- randomForest(x = x, y = y
                     , data=traindata,
                     importance=TRUE,
                     # fit 2000 decision trees!
                     ntree=50)
fit4
varImpPlot(fit4)
Prediction4 <- predict(fit4, testdata, type = "response")
pred4<-prediction(Prediction4$predictions, Prediction4$labels)
Actual <- testdata$Churn
confusionMatrix(reference = Actual, data = Prediction4)
confusionMatrix(reference = Actual, data = Prediction3)


library(ROCR)
