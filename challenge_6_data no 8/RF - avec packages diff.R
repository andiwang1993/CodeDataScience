data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
nums <- sapply(data, is.numeric)
data <- data[, nums]
datatest <- read.csv2('TestSample.csv')
for(i in 1:ncol(datatest)){
  datatest[is.na(datatest[,i]), i] <- mean(datatest[,i], na.rm = TRUE)
}
nums <- sapply(datatest, is.numeric)
datatest <- datatest[, nums]
yGas<- data$GasCum360
yOil<-data$OilCum360
x <- data[, 1:(ncol(data)-2)]

x <- data.frame( x)

n <- dim(x)[1]
trainIndex <- sample(n, floor(n*2/3))  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)

xtrain<-x[trainIndex,]
xnew<-x[testIndex,]

##utiliser package randomforest et RFinfer pour prediction des intervalles de confiance
##sans boostrap
library(randomForest)
library(RFinfer)
modelRFGas<-randomForest(yGas[trainIndex]~.,data=x[trainIndex,],keep.inbag=TRUE,ntree=500)

predGas2<-rfPredVar(modelRFGas,pred.data=xnew,CI=TRUE,tree.type='rf')
plot(predGas2$pred,yGas[testIndex])
plot(modelRFGas)
modelRFOil<-randomForest(yOil[trainIndex]~.,data=x[trainIndex,],keep.inbag=TRUE,ntree=500)
predOil2<-rfPredVar(modelRFOil,pred.data=xnew,CI=TRUE,tree.type='rf')
plot(predOil2$pred,yOil[testIndex])
plot(modelRFGas)


##utiliser package grf et le boostrap pour prediction
library(grf)
###sans bootstrap et sans Cross-validation
model1Gas<-regression_forest(as.matrix(x),yGas)
predGas1<-predict(model1Gas,x,estimate.variance = TRUE)
plot(predGas1$predictions,yGas)

ICGas<-cbind(predGas1$predictions,predGas1$predictions+qnorm(0.025)*predGas1$variance.estimates,predGas1$predictions+qnorm(0.975)*predGas1$variance.estimates)

model1Oil<-regression_forest(as.matrix(x),yOil)
predOil1<-predict(model1Oil,x,estimate.variance = TRUE)
plot(predOil1$predictions,yOil)

ICOil<-cbind(predOil1$predictions,predOil1$predictions+qnorm(0.025)*predOil1$variance.estimates,predOil1$predictions+qnorm(0.975)*predOil1$variance.estimates)
####boostrap
rfPredBoot <- function(x, y, xnew,nboot = 100){
  n <- length(x[,1])
  nNew <- length(xnew[,1])
  bootPred <- bootConf <- matrix(NA, nrow = nNew, ncol = nboot)
  for (i in 1:nboot){
    index <- sample(n, replace = TRUE)
    mboot <- regression_forest(X = as.matrix(x[index,]), Y = y[index])
    pred<-predict(mboot, newdata=xnew,estimate.variance = TRUE)
    pboot <- pred$predictions
    bootConf[, i] <- pboot[, 1]
    sdBoot <- pred$variance.estimates
    bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sdBoot)
  
  }
  pboot <- matrix(NA, nrow = nNew, ncol = 5)
  pboot[, 1] <- rowMeans(bootConf)
  sdConfBoot <- apply(bootConf, 1, sd)
  sdPredBoot <- apply(bootPred, 1, sd)
  pboot[, 4] <- pboot[, 1] +qnorm(0.025)* sdConfBoot # apply(Mboot, 1, quantile, 0.025)
  pboot[, 5] <- pboot[, 1] + qnorm(0.975)* sdConfBoot # apply(Mboot, 1, quantile, 0.975)
  pboot[, 2] <- pboot[, 1] +qnorm(0.025)*sdPredBoot # apply(Mboot, 1, quantile, 0.025)
  pboot[, 3] <- pboot[, 1] + qnorm(0.975)* sdPredBoot # apply(Mboot, 1, quantile, 0.975)
  return(pboot)
}
###

predGas3<-rfPredBoot (xtrain, yGas[trainIndex], xnew,nboot = 10)
predOil3<-rfPredBoot (xtrain, yOil[trainIndex], xnew,nboot = 10)
plot(predGas3[,1],yGas[testIndex])
plot(predOil3[,1],yOil[testIndex])


