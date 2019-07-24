data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

nums <- sapply(data, is.numeric)
data <- data[ , nums] # variables numeriques uniquement
data$API <- NULL       

matGas<-cbind(data[,1:41],data[,42])
matOil<-cbind(data[,1:41],data[,43])

###regression sur ACP

gas.cp<-princomp(matGas)
summary(gas.cp)
gas.cp$sdev^2##valeurs propres
###choisir valeurs propres superieure a 1 [1:10]

modelgas<-lm(data[,42]~gas.cp$scores[,1]+gas.cp$scores[,2]+gas.cp$scores[,3]+gas.cp$scores[,4]+gas.cp$scores[,5]+gas.cp$scores[,6]+gas.cp$scores[,7]+gas.cp$scores[,8]+gas.cp$scores[,9]+gas.cp$scores[,10])
summary(modelgas)

Oil.cp<-princomp(matOil)
summary(Oil.cp)
Oil.cp$sdev^2##valeurs propres
###choisir valeurs propres superieure a 1 [1:11]

modelOil<-lm(data[,43]~Oil.cp$scores[,1]+Oil.cp$scores[,2]+Oil.cp$scores[,3]+Oil.cp$scores[,4]+Oil.cp$scores[,5]+Oil.cp$scores[,6]+Oil.cp$scores[,7]+Oil.cp$scores[,8]+Oil.cp$scores[,9]+Oil.cp$scores[,10]+Oil.cp$scores[,11])
summary(modelgas)
#######sans library

#Centralisation/Normalisation
n<-460
trainindex<-sample(n,360)
testindex<-setdiff(1:n,trainindex)
v_gas <- scale(matGas, scale = FALSE)
v_oil<-scale(matOil,scale=FALSE)
v_gas.train<-v_gas[trainindex,]
v_oil.train<-v_oil[trainindex,]
v_gas.test<-v_gas[testindex,]
v_oil.test<-v_oil[testindex,]
###
covgas<-cov(v_gas.train)
covoil<-cov(v_oil.train)

gas.eigen<-eigen(covgas)
gas.eigen$values
proj_gas<-v_gas.train[,1:41]%*%gas.eigen$vectors[1:41,1:10]
modelgas<-lm(data[trainindex,42]~.,data.frame(proj_gas))
#summary(modelgas)

oil.eigen<-eigen(covoil)
oil.eigen$values
proj_oil<-v_oil.train[,1:41]%*%oil.eigen$vectors[1:41,1:11]
modelOil<-lm(data[trainindex,43]~.,data.frame(proj_oil))
#summary(modelgas)
pregas<-matrix(0,100,3)
preoil<-matrix(0,100,3)
proj_gas.test<-v_gas.test[,1:41]%*%gas.eigen$vectors[1:41,1:10]
proj_oil.test<-v_oil.test[,1:41]%*%gas.eigen$vectors[1:41,1:11]
predgas<-predict(modelgas,data.frame(proj_gas.test),se.fit = TRUE)
predoil<-predict(modelOil,data.frame(proj_oil.test),se.fit = TRUE)
# score_min<-10
# for (i1 in 1:10){
#   for(i2 in 5:15){
#     for(j1 in 10:20){
#       for(j2 in 10:20){
q1_inf=0.10
q1_sup=0.10
q2_inf=0.10
q2_sup=0.10
pregas[,1]<-predgas$fit
pregas[,2]<-pregas[,1]+qt(q1_inf,df=predgas$df)*sqrt(predgas$se.fit^2 + predgas$residual.scale^2)
pregas[,3]<-pregas[,1]+qt(1-q1_sup,df=predgas$df)*sqrt(predgas$se.fit^2 + predgas$residual.scale^2)

preoil[,1]<-predoil$fit
preoil[,2]<-preoil[,1]+qt(q2_inf,df=predoil$df)*sqrt(predoil$se.fit^2 + predoil$residual.scale^2)
preoil[,3]<-preoil[,1]+qt(1-q2_sup,df=predoil$df)*sqrt(predoil$se.fit^2 + predoil$residual.scale^2)



score.fun <- function(ktest,ICprevGas,ICprevOil,data.test) {
  score <- 0
  for (k in 1:ktest) {
    if ( (ICprevGas[k,2] > data.test[k,42]) | (ICprevGas[k,3] < data.test[k,42]) |
         (ICprevOil[k,2] > data.test[k,43]) | (ICprevOil[k,3] < data.test[k,43]) )  
      score <- score + 10
    else
      score <- score + (ICprevGas[k,3]-ICprevGas[k,2])*(ICprevOil[k,3]-ICprevOil[k,2])
  }
  score <- score/ktest
  return(score)
}
ktest <- 100
score1 <- score.fun(ktest,pregas,preoil,data[testindex,])
score1

# if(score<score_min){
#   score_min<-score
#   min_i1<-i1
#   min_i2<-i2
#   min_j1<-j1
#   min_j2<-j2
# }
#       }
#     }
#   }
# }
#avec boostrap

rfPredBoot <- function(x, y, xnew,q1,q2,nboot){
  n <- length(x[,1])
  nNew <- length(xnew[,1])
  bootPred <- bootConf <- matrix(NA, nrow = nNew, ncol = nboot)
  pboot <- matrix(NA, nrow = nNew, ncol = 3)
  
  for (i in 1:nboot){
    index <- sample(n, replace = TRUE)
    mboot <- lm(y[index]~.,data.frame(x[index,]))
    pred<-predict(mboot, newdata=data.frame(xnew),se.fit=TRUE)
    pboot[,1] <- pred$fit
    bootConf[, i] <- pboot[, 1]
    
    # sdBoot <- sd(predict(mboot,data.frame(x[index,]),se.fit=TRUE)$fit-y[index])
    sdBoot<-sqrt(pred$se.fit^2 + pred$residual.scale^2)
    bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sdBoot)
    
  }
  
  pboot[, 1] <- rowMeans(bootConf)
  # sdConfBoot <- apply(bootConf, 1, sd)
  sdPredBoot <- apply(bootPred, 1, sd)
  # pboot[, 4] <- pboot[, 1] +qnorm(q1_inf)* sdConfBoot # apply(Mboot, 1, quantile, 0.025)
  # pboot[, 5] <- pboot[, 1] + qnorm(1-q1_sup)* sdConfBoot # apply(Mboot, 1, quantile, 0.975)
  pboot[, 2] <- pboot[, 1] +qt(q1,df=pred$df)*sdPredBoot # apply(Mboot, 1, quantile, 0.025)
  pboot[, 3] <- pboot[, 1] + qt(1-q2,df=pred$df)* sdPredBoot # apply(Mboot, 1, quantile, 0.975)
  return(pboot)
}
##########
predgas2<-rfPredBoot (proj_gas, data[trainindex,42], proj_gas.test,0.06,0.19,500)
predoil2<-rfPredBoot (proj_oil, data[trainindex,43], proj_oil.test,0.15,0.17,500)

plot(predgas2[,1],data[testindex,42])
score2 <- score.fun(ktest,predgas2,predoil2,data[testindex,])
score2
print(score1)
print(score2)

