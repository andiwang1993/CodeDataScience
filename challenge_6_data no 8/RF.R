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
trainIndex <- sample(n, 360)  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)
#testIndex<-sample(n,1000,replace = TRUE)
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
model1Gas<-regression_forest(as.matrix(x[trainIndex,]),yGas[trainIndex])
predGas1<-predict(model1Gas,xnew,estimate.variance = TRUE)
plot(predGas1$predictions,yGas[testIndex])

ICGas<-cbind(predGas1$predictions,predGas1$predictions+qnorm(0.10)*predGas1$variance.estimates,predGas1$predictions+qnorm(0.9)*predGas1$variance.estimates)

model1Oil<-regression_forest(as.matrix(x[trainIndex,]),yOil[trainIndex])
predOil1<-predict(model1Oil,xnew,estimate.variance = TRUE)
plot(predOil1$predictions,yOil[testIndex])

ICOil<-cbind(predOil1$predictions,predOil1$predictions+qnorm(0.10)*predOil1$variance.estimates,predOil1$predictions+qnorm(0.9)*predOil1$variance.estimates)


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
    
    sdBoot <- sd(predict(mboot)$predictions-y[index])
    bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sdBoot)
  
  }
  pboot <- matrix(NA, nrow = nNew, ncol = 5)
  pboot[, 1] <- rowMeans(bootConf)
  sdConfBoot <- apply(bootConf, 1, sd)
  sdPredBoot <- apply(bootPred, 1, sd)
  pboot[, 4] <- pboot[, 1] +qnorm(0.1)* sdConfBoot # apply(Mboot, 1, quantile, 0.025)
  pboot[, 5] <- pboot[, 1] + qnorm(0.9)* sdConfBoot # apply(Mboot, 1, quantile, 0.975)
  pboot[, 2] <- pboot[, 1] +qnorm(0.1)*sdPredBoot # apply(Mboot, 1, quantile, 0.025)
  pboot[, 3] <- pboot[, 1] + qnorm(0.9)* sdPredBoot # apply(Mboot, 1, quantile, 0.975)
  return(pboot)
}
###

predGas3<-rfPredBoot (x[trainIndex,], yGas[trainIndex], xnew,nboot = 20)
predOil3<-rfPredBoot (x[trainIndex,], yOil[trainIndex], xnew,nboot = 20)
plot(predGas3[,1],yGas[testIndex])
plot(predOil3[,1],yOil[testIndex])

score.fun <- function(ktest,ICprevGas,ICprevOil,data.test) {
  score <- 0
  for (k in 1:ktest) {
    if ( (ICprevGas[k,2] > data.test[k,43]) | (ICprevGas[k,3] < data.test[k,43]) |
         (ICprevOil[k,2] > data.test[k,44]) | (ICprevOil[k,3] < data.test[k,44]) )  
      score <- score + 10
    else
      score <- score + (ICprevGas[k,3]-ICprevGas[k,2])*(ICprevOil[k,3]-ICprevOil[k,2])
  }
  score <- score/ktest
  return(score)
}
test<-data[testIndex,]

score.fun(nrow(test),predGas3[,1:3],predOil3[,1:3],test)


