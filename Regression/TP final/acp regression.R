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
v_gas <- scale(matGas, scale = FALSE)
v_oil<-scale(matOil,scale=FALSE)
###
covgas<-cov(v_gas)
covoil<-cov(v_oil)

gas.eigen<-eigen(covgas)
gas.eigen$values
proj_gas<-v_gas%*%gas.eigen$vectors[,1:10]
modelgas<-lm(data[,42]~.,data.frame(proj_gas))
#summary(modelgas)

oil.eigen<-eigen(covoil)
oil.eigen$values
proj_oil<-v_oil%*%oil.eigen$vectors[,1:11]
modelOil<-lm(data[,43]~.,data.frame(proj_oil))
#summary(modelgas)
pregas<-matrix(0,100,3)
preoil<-matrix(0,100,3)
predgas<-predict(modelgas,data.frame(proj_gas[ind.test,]),se.fit = TRUE)
predoil<-predict(modelOil,data.frame(proj_oil[ind.test,]),se.fit = TRUE)
score_min<-10
for (i1 in 1:10){
  for(i2 in 5:15){
    for(j1 in 10:20){
      for(j2 in 10:20){
q1_inf=i1/100
q1_sup=i2/100
q2_inf=j1/100
q2_sup=j2/100
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
score <- score.fun(ktest,pregas,preoil,data.test)
score

if(score<score_min){
  score_min<-score
  min_i1<-i1
  min_i2<-i2
  min_j1<-j1
  min_j2<-j2
}
      }
    }
  }
}
