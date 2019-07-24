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
test <- read.csv2('TestSample.csv')
test<-test[,-4:-6]
for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
}

nums <- sapply(test, is.numeric)
test <- test[ , nums] #
test<-scale(test)

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
projtestgas<-test%*%gas.eigen$vectors[1:42,1:10]
projtesoil<-test%*%oil.eigen$vectors[1:42,1:11]
pregas<-matrix(0,235,3)
preoil<-matrix(0,235,3)
predgas<-predict(modelgas,data.frame(projtestgas),se.fit = TRUE)
predoil<-predict(modelOil,data.frame(projtesoil),se.fit = TRUE)

        q1_inf=0.06
        q1_sup=0.12
        q2_inf=0.15
        q2_sup=0.17
        pregas[,1]<-predgas$fit
        pregas[,2]<-pregas[,1]+qt(q1_inf,df=predgas$df)*sqrt(predgas$se.fit^2 + predgas$residual.scale^2)
        pregas[,3]<-pregas[,1]+qt(1-q1_sup,df=predgas$df)*sqrt(predgas$se.fit^2 + predgas$residual.scale^2)
        
        preoil[,1]<-predoil$fit
        preoil[,2]<-preoil[,1]+qt(q2_inf,df=predoil$df)*sqrt(predoil$se.fit^2 + predoil$residual.scale^2)
        preoil[,3]<-preoil[,1]+qt(1-q2_sup,df=predoil$df)*sqrt(predoil$se.fit^2 + predoil$residual.scale^2)
        
        test1 <- read.csv2('TestSample.csv')
        results <- data.frame(ID = test1$API,
                              GAS360_INF = pregas[,2],
                              GAS360_SUP = pregas[,3],
                              CUM360_INF = preoil[,2],
                              CUM360_SUP = preoil[,3]
        ) 
        
        
        write.table(results, "submit1.csv", sep = ";",
                    quote = FALSE, row.names = FALSE)
        
        
        
       