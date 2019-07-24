# # TP final : Régression Linéaire pour le challenge Total
# # Majeure Data Science 2017-2018
# 
# # Sélection de la variable la plus corrélée avec chacune des deux variables
# # ? prédire et régression linéaire simple (rls : p = 1 prédicteur seulement)
# 
# # chargement des données et nettoyage
# data <- read.csv2('TrainSample.csv')
# nums <- sapply(data, is.numeric) 
# data <- data[ , nums] # variables numériques uniquement
# data$API <- NULL       # on enlève l'identifiant
# 
# # les deux variables ? prédire (deux dernières variables en colonne de data)
# # Gas <- data[,42]
# # Oil <- data[,43]
# 
# # data test et training data 
# 
# ind.test <- sample(1:460, size=100, replace = FALSE)
# data.test <- data[ind.test,]
# data.train <- data[-ind.test,]


# # sélection de la variable la plus corrélée avec Gas
# C <- cor(data,use="complete.obs")
# indPredGas <- which.max(abs(C[1:41,42]))
# names(data)[indPredGas]
# 
# # sélection de la variable la plus corrélée avec Oil
# C <- cor(data,use="complete.obs")
# indPredOil <- which.max(abs(C[1:41,43]))
# names(data)[indPredOil]


# jeux de données d'apprentissage correspondant
# pour prédire Gas ? partir d'une rls
dataGas <- data.frame(Depth = data.train[, indPredGas], Gas = data.train[, 42])
# pour prédire Oil ? partir d'une rls
dataOil <- data.frame(Zone = data.train[, indPredOil], Oil = data.train[, 43])


#############################################################################
###  Préliminaires : Calcul d'un score de référence sans utilisation
###                  de prédicteurs (p = 0)
#############################################################################

Gas <- dataGas$Gas
Oil <- dataOil$Oil

conf <- 0.7
qGasInf <- quantile(Gas,(1-conf)/2)
qGasSup <- quantile(Gas,1-(1-conf)/2)
qOilInf <- quantile(Oil,(1-conf)/2)
qOilSup <- quantile(Oil,1-(1-conf)/2)
plot(Gas,Oil,ylim=c(-1.5,6))
segments(qGasInf,qOilInf,qGasSup,qOilInf,col='red',lwd=2)
segments(qGasInf,qOilSup,qGasSup,qOilSup,col='red',lwd=2)
segments(qGasInf,qOilInf,qGasInf,qOilSup,col='red',lwd=2)
segments(qGasSup,qOilInf,qGasSup,qOilSup,col='red',lwd=2)
cond <- ( qGasInf < data.test[,42] & data.test[,42] < qGasSup &
            qOilInf < data.test[,43] & data.test[,43] < qOilSup)
points(data.test[,42],data.test[,43],col='blue')
points(data.test[cond,42],data.test[cond,43],col='red')
prop <- sum(cond)/100
score <- prop*(qGasSup-qGasInf)*(qOilSup-qOilInf) + (1-prop)*10
print(qGasSup-qGasInf)
print(qOilSup-qOilInf)
print(score)

# Vu la dissymétrie importante des distributions de Gas et Oil, on choisit
# des intervalles de prédiction non symétriques!

p_inf <- 0.05
p_sup <- 0.2
qGasInf <- quantile(Gas,p_inf)
qGasSup <- quantile(Gas,1-p_sup)
qOilInf <- quantile(Oil,p_inf)
qOilSup <- quantile(Oil,1-p_sup)
plot(Gas,Oil,ylim=c(-1.5,6))
segments(qGasInf,qOilInf,qGasSup,qOilInf,col='red',lwd=2)
segments(qGasInf,qOilSup,qGasSup,qOilSup,col='red',lwd=2)
segments(qGasInf,qOilInf,qGasInf,qOilSup,col='red',lwd=2)
segments(qGasSup,qOilInf,qGasSup,qOilSup,col='red',lwd=2)
cond <- ( qGasInf < data.test[,42] & data.test[,42] < qGasSup &
            qOilInf < data.test[,43] & data.test[,43] < qOilSup)
points(data.test[,42],data.test[,43],col='blue')
points(data.test[cond,42],data.test[cond,43],col='red')
prop <- sum(cond)/100
score <- prop*(qGasSup-qGasInf)*(qOilSup-qOilInf) + (1-prop)*10
print(qGasSup-qGasInf)
print(qOilSup-qOilInf)
print(score)

#############################################################################
###  Modèles de régression linéaire simple : Gas ~ Depth  et Oil ~ Zone
#############################################################################


# on commence par Gas ~ Depth

Gas <- dataGas$Gas
Depth <- dataGas$Depth

plot(Gas ~ Depth,xlab="Normalized True Vertical Depth of the reservoir (feet)",ylab="Cumulative gas volume after 360 days of production")
regGas <- lm(Gas ~ Depth)
print(regGas)
abline(regGas,col="red",lwd=2)
regGas.s <- summary(regGas)
print(regGas.s)
title('Régression linéaire simple Gas ~ Depth')


# estimation + prédiction : permet de juger graphiquement
# de la qualit? du modèle en termes d'estimation et de prédiction

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
Depthnew <- seq(min(Depth),max(Depth),length=100)
grille <- data.frame(Depth=Depthnew)
ICdte <- predict(regGas,new=grille,interval="conf",level=0.95)
ICprev <- predict(regGas,new=grille,interval="pred",level=0.95)
matlines(Depthnew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la réponse moyenne et intervalle de prédiction')

# analyse des résidus

plot(rstudent(regGas) ~ fitted(regGas),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('Résidus studentisés contre la réponse prédite')

# autre outils de validation : droite de Henry pour la normalit? des résidus
plot(regGas,which=2)
abline(0,1,col="red",lwd=2)

# cas de la rls : Oil ~ Zone

Oil <- dataOil$Oil
Zone <- dataOil$Zone

plot(Oil ~ Zone,xlab="Normalized specific Zone of Production",ylab="Cumulative oil volume after 360 days of production")
regOil <- lm(Oil ~ Zone)
print(regOil)
abline(regOil,col="red",lwd=2)
regOil.s <- summary(regOil)
print(regOil.s)
title('Régression linéaire simple Oil ~ Zone')


# estimation + prédiction : permet de juger graphiquement
# de la qualit? du modèle en termes d'estimation et de prédiction

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
Zonenew <- seq(min(Zone),max(Zone),length=100)
grille <- data.frame(Zone=Zonenew)
ICdte <- predict(regOil,new=grille,interval="conf",level=0.95)
ICprev <- predict(regOil,new=grille,interval="pred",level=0.95)
matlines(Zonenew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la réponse moyenne et intervalle de prédiction')

# analyse des résidus

plot(rstudent(regOil) ~ fitted(regOil),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('Résidus studentisés contre la réponse prédite')

# autre outils de validation : droite de Henry pour la normalit? des résidus
plot(regOil,which=2)
abline(0,1,col="red",lwd=2)

# fonction R pour calculer le score

score.fun <- function(ktest,ICprevGas,ICprevOil,data.test) {
  score <- 0
  for (k in 1:ktest) {
    if ( (ICprevGas[k,"lwr"] > data.test[k,42]) | (ICprevGas[k,"upr"] < data.test[k,42]) |
       (ICprevOil[k,"lwr"] > data.test[k,43]) | (ICprevOil[k,"upr"] < data.test[k,43]) )  
      score <- score + 10
    else
      score <- score + (ICprevGas[k,"upr"]-ICprevGas[k,"lwr"])*(ICprevOil[k,"upr"]-ICprevOil[k,"lwr"])
  }
  score <- score/ktest
  return(score)
}

# calcul des intervalles de prediction pour Gas et Oil

Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevGas <- predict(regGas,new=Depth.new,interval="pred",level=0.7)
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevOil <- predict(regOil,new=Zone.new,interval="pred",level=0.7)



####
Depth.new <- data.frame(Depth=data.test[,indPredGas])
prevGas <- predict(regGas,new=Depth.new,se.fit = TRUE)
Zone.new <- data.frame(Zone=data.test[,indPredOil])
prevOil <- predict(regOil,new=Zone.new,se.fit = TRUE)

# score_min<-10
# for (i1 in 10:20){
#   for(i2 in 10:20){
#     for(j1 in 10:20){ 
#       for(j2 in 20:30){

ICprevGas<-matrix(0,100,3)
ICprevOil<-matrix(0,100,3)
ICprevGas[,1]<-prevGas$fit
q1_inf<-0.12
q1_sup<-0.13
ICprevGas[,2]<-ICprevGas[,1]+qt(q1_inf,df=prevGas$df)*sqrt(prevGas$se.fit^2 + prevGas$residual.scale^2)
ICprevGas[,3]<-ICprevGas[,1]+qt(1-q1_sup,df=prevGas$df)*sqrt(prevGas$se.fit^2 + prevGas$residual.scale^2)
colnames(ICprevGas)=c("fit","lwr","upr")
q2_inf<-0.15
q2_sup<-0.24
ICprevOil[,2]<-ICprevOil[,1]+qt(q2_inf,df=prevOil$df)*sqrt(prevOil$se.fit^2 + prevOil$residual.scale^2)
ICprevOil[,3]<-ICprevOil[,1]+qt(1-q2_sup,df=prevOil$df)*sqrt(prevOil$se.fit^2 + prevOil$residual.scale^2)
colnames(ICprevOil)=c("fit","lwr","upr")

# Visualisation prédictions pour la variable Gas

# plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
# abline(regGas,lwd=2,col='blue')
# points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
# segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
# title('Intervalles de prédiction et données test')
# 
# # Visualisation prédictions pour la variable Oil
# 
# plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
# abline(regOil,lwd=2,col='blue')
# points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
# segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
# title('Intervalles de prédiction et données test')

# valeur du score 
# attention au fait que cette valeur peut être supérieure ? 10 car les intervalles de prédiction
# utilisés peuvent conduire ? une surface supérieure ? 10  

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)
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

#############################################################################
# Amélioration du modèle de régression Gas ~ Depth
#############################################################################

plot(Gas ~ Depth,xlab="Depth",ylab="Gas")
regGas <- lm(Gas ~ Depth)
abline(regGas,col="red",lwd=2)
title('Gas contre Depth et droite aux moindres carrés')

# on devine une valeur limite inférieure pour la variable ? prédire Gas

Gasinf <- min(Gas)
print(Gasinf)

# il est alors tentant d'essayer une transformation du type log(Gas - seuil) avec seuil ? faire varier (estimer ?)

seuil.G = Gasinf - 0.005     # pour éviter ln(0) = +infini

logGas <- log(Gas - seuil.G)
plot(logGas ~ Depth,xlab="Depth",ylab="logGas")
reglogGas <- lm(logGas ~ Depth)
abline(reglogGas,col="red",lwd=2)


# C'est déj? beaucoup mieux mais on peut encore améliorer

reglogGas <- lm(logGas ~ Depth + I(Depth^2))
indices <- order(Depth)
lines(Depth[indices],reglogGas$fitted.values[indices],col="blue",lwd=2)
title('logGas contre Depth et régression linéaire multiple')

#############################################################################
# Amélioration du modèle de régression Oil ~ Zone
#############################################################################

plot(Oil ~ Zone,xlab="Zone",ylab="Oil")
regOil <- lm(Oil ~ Zone)
abline(regOil,col="red",lwd=2)
title('Oil contre Zone et droite aux moindres carrés')

#  on devine ? nouveau une valeur limite inférieure pour la variable Oil

Oilinf <- min(Oil)
print(Oilinf)

# il est tentant d'essayer une transformation du type log(Gas - seuil) avec seuil ? faire varier (estimer ?)

seuil.O = Oilinf - 0.005     # pour éviter ln(0) = +infini

logOil <- log(Oil - seuil.O)
plot(logOil ~ Zone,xlab="Zone",ylab="logOil")
reglogOil <- lm(logOil ~ Zone)
abline(reglogOil,col="red",lwd=2)


# on peut encore améliorer

reglogOil <- lm(logOil ~ Zone + I(Zone^2))
indices <- order(Zone)
lines(Zone[indices],reglogOil$fitted.values[indices],col="blue",lwd=2)
title('logOil contre Zone et régression linéaire multiple')

 
# Calcul du score avec le modèle reglogGas pour prédire Gas et reglogOil pour prédire Oil


Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevlogGas <- predict(reglogGas,new=Depth.new,interval="pred",level=0.7)
# attention ? bien utiliser la tranformation inverse
ICprevGas <- exp(ICprevlogGas) + seuil.G
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevlogOil <- predict(reglogOil,new=Zone.new,interval="pred",level=0.7)
ICprevOil <- exp(ICprevlogOil) + seuil.O



prevlogGas <- predict(reglogGas,new=Depth.new,se.fit = TRUE)
prevlogOil <- predict(reglogOil,new=Zone.new,se.fit = TRUE)
ICprevGas<-matrix(0,100,3)
ICprevOil<-matrix(0,100,3)

# score_min<-10
# for (a1 in 10:20){
#   for(a2 in 5:15){
#     for(b1 in 0:5){
#       for(b2 in 15:25){

ICprevlogGas[,1]<-prevlogGas$fit
q1_inf<-0.12
q1_sup<-9/100
ICprevlogGas[,2]<-ICprevlogGas[,1]+qt(q1_inf,df=prevlogGas$df)*sqrt(prevlogGas$se.fit^2 + prevlogGas$residual.scale^2)
ICprevlogGas[,3]<-ICprevlogGas[,1]+qt(1-q1_sup,df=prevlogGas$df)*sqrt(prevlogGas$se.fit^2 + prevlogGas$residual.scale^2)
colnames(ICprevGas)=c("fit","lwr","upr")
ICprevlogOil[,1]<-prevlogOil$fit
q2_inf<-0/100
q2_sup<-23/100
ICprevlogOil[,2]<-ICprevlogOil[,1]+qt(q2_inf,df=prevlogOil$df)*sqrt(prevlogOil$se.fit^2 + prevlogOil$residual.scale^2)
ICprevlogOil[,3]<-ICprevlogOil[,1]+qt(1-q2_sup,df=prevlogOil$df)*sqrt(prevlogOil$se.fit^2 + prevlogOil$residual.scale^2)
colnames(ICprevOil)=c("fit","lwr","upr")

ICprevGas <- exp(ICprevlogGas) + seuil.G
ICprevOil <- exp(ICprevlogOil) + seuil.O

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)
# if(score<score_min){
#   score_min<-score
#   min_a1<-a1
#   min_a2<-a2
#   min_b1<-b1
#   min_b2<-b2
# }
#       }
#     }
#   }
# }



# Visualisation prédictions pour la variable Gas

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
title('Intervalles de prediction et donnees test')

# Visualisation predictions pour la variable Oil

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
title('Intervalles de prediction et donnees test')


######################################

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
nums <- sapply(data, is.numeric)
data <- data[, nums]
y1<- data$GasCum360
x1 <- data[, 1:(ncol(data)-2)]

df1 <- data.frame(x1)
df1 <- na.omit(df1)

n <- dim(df1)[1]
trainIndex <- sample(n, floor(n/2))  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)

Gasmodel <- lm(y1~ ., df1, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)
df11<-df1[,c(-1,-2,-3,-4,-6,-9,-13,-14,-17,-25,-28,-32,-34)]
Gasmodel <- lm(y1~ ., df11, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)
df12<-df11[,c(-2,-3,-4,-6,-8,-9,-12,-13,-14,-15,-17,-19,-21,-27)]
Gasmodel <- lm(y1~ ., df12, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)
df13<-df12[,c(-2,-3,-4,-6,-9)]
Gasmodel <- lm(y1~ ., df13, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)

df14<-df13[,c(-2,-3,-4,-5,-9)]
Gasmodel <- lm(y1~ ., df14, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)

df15<-df14[,-2]
Gasmodel <- lm(y1~ ., df15, subset = trainIndex)
summary(Gasmodel)

anova(Gasmodel)
y2<- data$OilCum360
Oilmodel <- lm(y2~ ., df1, subset = trainIndex)
summary(Oilmodel)

anova(Oilmodel)
df21<-df1[,c(-1,-2,-3,-4,-7,-11,-13,-17,-24,-26)]
Oilmodel <- lm(y2~ ., df21, subset = trainIndex)
summary(Oilmodel)

anova(Oilmodel)

df22<-df21[,c(-1,-3,-4,-6,-7,-9,-10,-15,-17)]
Oilmodel <- lm(y2~ ., df22, subset = trainIndex)
summary(Oilmodel)

anova(Oilmodel)

df23<-df22[,c(-2,-4,-5,-6,-9,-13,-15,-17,-18,-21)]
Oilmodel <- lm(y2~ ., df23, subset = trainIndex)
summary(Oilmodel)

anova(Oilmodel)
df24<-df23[,c(-4,-12)]
Oilmodel <- lm(y2~ ., df24, subset = trainIndex)
summary(Oilmodel)

anova(Oilmodel)

predicteurGas.new <- data.frame(data.test$Erosion_PPLS..ft.,data.test$Shot_Density..shots.ft.,data.test$Shot_Total)
colnames(predicteurGas.new)<-c("Erosion_PPLS..ft.","Shot_Density..shots.ft.","Shot_Total")
for(i in 1:ncol(predicteurGas.new)){
  predicteurGas.new[is.na(predicteurGas.new[,i]), i] <- mean(predicteurGas.new[,i], na.rm = TRUE)
}

NprevGas <- predict(Gasmodel,new=predicteurGas.new,se.fit = TRUE)

predicteurOil.new<-data.frame(data.test[,"Pressure_PPLS..PSI."], data.test[,"GR_PPLS..API."],data.test[,"Heat_Flow..W.m2."],data.test[,"Avg_Treating_Pressure..KPa."],data.test[,"Max_Treating_pressure..KPa."],data.test[,"Min_Treating_Pressure..KPa."],data.test[,"Max_Rate_Slurry..bpm."],data.test[,"ShutInPressure_Fil..KPa."],data.test[,"Shot_Density..shots.ft."],data.test[,"Shot_Total"])
colnames(predicteurOil.new)<-c("Pressure_PPLS..PSI.", "GR_PPLS..API.", "Heat_Flow..W.m2.","Avg_Treating_Pressure..KPa.","Max_Treating_pressure..KPa.","Min_Treating_Pressure..KPa.","Max_Rate_Slurry..bpm.","ShutInPressure_Fil..KPa.","Shot_Density..shots.ft.","Shot_Total")      

for(i in 1:ncol(predicteurOil.new)){
  predicteurOil.new[is.na(predicteurOil.new[,i]), i] <- mean(predicteurOil.new[,i], na.rm = TRUE)
}
nums <- sapply(predicteurOil.new, is.numeric)
predicteurOil.new <- predicteurOil.new[, nums]
NprevOil <- predict(Oilmodel,new=predicteurOil.new,se.fit = TRUE)


score_min<-10
for (i1 in 10:20){
  for(i2 in 20:30){
    for(j1 in 15:25){
      for(j2 in 25:30){
        NICprevGas<-matrix(0,100,3)
        NICprevOil<-matrix(0,100,3)

NICprevGas[,1]<-NprevGas$fit
q1_inf<-i1/100
q1_sup<-i2/100
NICprevGas[,2]<-NICprevGas[,1]+qt(q1_inf,df=NprevGas$df)*sqrt(NprevGas$se.fit^2 + NprevGas$residual.scale^2)
NICprevGas[,3]<-ICprevGas[,1]+qt(1-q1_sup,df=prevGas$df)*sqrt(NprevGas$se.fit^2 + NprevGas$residual.scale^2)
colnames(NICprevGas)=c("fit","lwr","upr")
q2_inf<-j1/100
q2_sup<-j2/100
NICprevOil[,2]<-NICprevOil[,1]+qt(q2_inf,df=NprevOil$df)*sqrt(NprevOil$se.fit^2 + NprevOil$residual.scale^2)
NICprevOil[,3]<-NICprevOil[,1]+qt(1-q2_sup,df=NprevOil$df)*sqrt(prevOil$se.fit^2 + NprevOil$residual.scale^2)
colnames(NICprevOil)=c("fit","lwr","upr")



ktest <- 100
score <- score.fun(ktest,NICprevGas,NICprevOil,data.test)
if(score<score_min){
  score_min<-score
  min_c1<-i1
  min_c2<-i2
  min_d1<-j1
  min_d2<-j2
}
      }
    }
  }
}

