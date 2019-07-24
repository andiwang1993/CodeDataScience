# TP final : R¨¦gression Lin¨¦aire pour le challenge Total
# Majeure Data Science 2017-2018

# S¨¦lection de la variable la plus corr¨¦l¨¦e avec chacune des deux variables
# ¨¤ pr¨¦dire et r¨¦gression lin¨¦aire simple (rls : p = 1 pr¨¦dicteur seulement)

# chargement des donn¨¦es et nettoyage
data <- read.csv2('TrainSample.csv')
nums <- sapply(data, is.numeric) 
data <- data[ , nums] # variables num¨¦riques uniquement
data$API <- NULL       # on enl¨¨ve l'identifiant

# les deux variables ¨¤ pr¨¦dire (deux derni¨¨res variables en colonne de data)
# Gas <- data[,42]
# Oil <- data[,43]

# data test et training data 

ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- data[ind.test,]
data.train <- data[-ind.test,]


# s¨¦lection de la variable la plus corr¨¦l¨¦e avec Gas
C <- cor(data,use="complete.obs")
indPredGas <- which.max(abs(C[1:41,42]))
names(data)[indPredGas]

# s¨¦lection de la variable la plus corr¨¦l¨¦e avec Oil
C <- cor(data,use="complete.obs")
indPredOil <- which.max(abs(C[1:41,43]))
names(data)[indPredOil]


# jeux de donn¨¦es d'apprentissage correspondant
# pour pr¨¦dire Gas ¨¤ partir d'une rls
dataGas <- data.frame(Depth = data.train[, indPredGas], Gas = data.train[, 42])
# pour pr¨¦dire Oil ¨¤ partir d'une rls
dataOil <- data.frame(Zone = data.train[, indPredOil], Oil = data.train[, 43])


#############################################################################
###  Pr¨¦liminaires : Calcul d'un score de r¨¦f¨¦rence sans utilisation
###                  de pr¨¦dicteurs (p = 0)
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

# Vu la dissym¨¦trie importante des distributions de Gas et Oil, on choisit
# des intervalles de pr¨¦diction non sym¨¦triques!
# inter<-matrix(0,2,1)
# scoremin<-10
# for (i in 0:100){
#   for (j in 100:i){
p_inf <- 0.02
p_sup <- 0.18
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
# if (score<scoremin){
#   scoremin<-score
#   inter[1]<-p_inf
#   inter[2]<-1-p_sup
# }
#   }
# }

print(qGasSup-qGasInf)
print(qOilSup-qOilInf)
print(score)

#############################################################################
###  Mod¨¨les de r¨¦gression lin¨¦aire simple : Gas ~ Depth  et Oil ~ Zone
#############################################################################


# on commence par Gas ~ Depth

Gas <- dataGas$Gas
Depth <- dataGas$Depth

plot(Gas ~ Depth,xlab="Normalized True Vertical Depth of the reservoir (feet)",ylab="Cumulative gas volume after 360 days of production")
##################
regGas <- lm(Gas ~ Depth)
print(regGas)
abline(regGas,col="red",lwd=2)
regGas.s <- summary(regGas)
print(regGas.s)
title('R¨¦gression lin¨¦aire simple Gas ~ Depth')


# estimation + pr¨¦diction : permet de juger graphiquement
# de la qualit¨¦ du mod¨¨le en termes d'estimation et de pr¨¦diction

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
Depthnew <- seq(min(Depth),max(Depth),length=100)
grille <- data.frame(Depth=Depthnew)
ICdte <- predict(regGas,new=grille,interval="conf",level=0.95)
ICprev <- predict(regGas,new=grille,interval="pred",level=0.95)
matlines(Depthnew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la r¨¦ponse moyenne et intervalle de pr¨¦diction')

# analyse des r¨¦sidus

plot(rstudent(regGas) ~ fitted(regGas),xlab="R¨¦ponse estim¨¦e",ylab="R¨¦sidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('R¨¦sidus studentis¨¦s contre la r¨¦ponse pr¨¦dite')

# autre outils de validation : droite de Henry pour la normalit¨¦ des r¨¦sidus
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
title('R¨¦gression lin¨¦aire simple Oil ~ Zone')


# estimation + pr¨¦diction : permet de juger graphiquement
# de la qualit¨¦ du mod¨¨le en termes d'estimation et de pr¨¦diction

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
Zonenew <- seq(min(Zone),max(Zone),length=100)
grille <- data.frame(Zone=Zonenew)
ICdte <- predict(regOil,new=grille,interval="conf",level=0.95)
ICprev <- predict(regOil,new=grille,interval="pred",level=0.95)
matlines(Zonenew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la r¨¦ponse moyenne et intervalle de pr¨¦diction')

# analyse des r¨¦sidus

plot(rstudent(regOil) ~ fitted(regOil),xlab="R¨¦ponse estim¨¦e",ylab="R¨¦sidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('R¨¦sidus studentis¨¦s contre la r¨¦ponse pr¨¦dite')

# autre outils de validation : droite de Henry pour la normalit¨¦ des r¨¦sidus
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
################################
# calcul des intervalles de pr¨¦diction pour Gas et Oil

Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevGas <- predict(regGas,new=Depth.new,interval="pred",level=0.70)
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevOil <- predict(regOil,new=Zone.new,interval="pred",level=0.70)

# Visualisation pr¨¦dictions pour la variable Gas

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
abline(regGas,lwd=2,col='blue')
points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
title('Intervalles de pr¨¦diction et donn¨¦es test')

# Visualisation pr¨¦dictions pour la variable Oil

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
abline(regOil,lwd=2,col='blue')
points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
title('Intervalles de pr¨¦diction et donn¨¦es test')

# valeur du score 
# attention au fait que cette valeur peut ¨ºtre superieure ¨¤ 10 car les intervalles de prédiction
# utilisee peuvent conduire ¨¤ une surface sup¨¦rieure ¨¤ 10  

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)





#############
regGas <- lm(Gas ~ Depth+Depth^2)
print(regGas)
abline(regGas,col="red",lwd=2)
regGas.s <- summary(regGas)
print(regGas.s)
title('R¨¦gression lin¨¦aire simple Gas ~ Depth')


# estimation + pr¨¦diction : permet de juger graphiquement
# de la qualit¨¦ du mod¨¨le en termes d'estimation et de pr¨¦diction

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
Depthnew <- seq(min(Depth),max(Depth),length=100)
grille <- data.frame(Depth=Depthnew)
ICdte <- predict(regGas,new=grille,interval="conf",level=0.95)
ICprev <- predict(regGas,new=grille,interval="pred",level=0.95)
matlines(Depthnew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la r¨¦ponse moyenne et intervalle de pr¨¦diction')

# analyse des r¨¦sidus

plot(rstudent(regGas) ~ fitted(regGas),xlab="R¨¦ponse estim¨¦e",ylab="R¨¦sidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('R¨¦sidus studentis¨¦s contre la r¨¦ponse pr¨¦dite')

# autre outils de validation : droite de Henry pour la normalit¨¦ des r¨¦sidus
plot(regGas,which=2)
abline(0,1,col="red",lwd=2)

# cas de la rls : Oil ~ Zone

Oil <- dataOil$Oil
Zone <- dataOil$Zone

plot(Oil ~ Zone,xlab="Normalized specific Zone of Production",ylab="Cumulative oil volume after 360 days of production")
regOil <- lm(Oil ~ Zone+Zone^2)
print(regOil)
abline(regOil,col="red",lwd=2)
regOil.s <- summary(regOil)
print(regOil.s)
title('R¨¦gression lin¨¦aire simple Oil ~ Zone')


# estimation + pr¨¦diction : permet de juger graphiquement
# de la qualit¨¦ du mod¨¨le en termes d'estimation et de pr¨¦diction

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
Zonenew <- seq(min(Zone),max(Zone),length=100)
grille <- data.frame(Zone=Zonenew)
ICdte <- predict(regOil,new=grille,interval="conf",level=0.95)
ICprev <- predict(regOil,new=grille,interval="pred",level=0.95)
matlines(Zonenew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))
title('Intervalle de confiance sur la r¨¦ponse moyenne et intervalle de pr¨¦diction')

# analyse des r¨¦sidus

plot(rstudent(regOil) ~ fitted(regOil),xlab="R¨¦ponse estim¨¦e",ylab="R¨¦sidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
abline(h=0,lty=2)
title('R¨¦sidus studentis¨¦s contre la r¨¦ponse pr¨¦dite')

# autre outils de validation : droite de Henry pour la normalit¨¦ des r¨¦sidus
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
################################
# calcul des intervalles de pr¨¦diction pour Gas et Oil

score_min<-10
for (i1 in 0:20){
  for(i2 in 50:80){
    for(j1 in 0:20){ 
  for(j2 in 50:80){

Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevGas <- predict(regGas,new=Depth.new,interval="pred",level=i1/100:i2/100)
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevOil <- predict(regOil,new=Zone.new,interval="pred",level=j1/100:j2/100)

# Visualisation pr¨¦dictions pour la variable Gas

# plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
# abline(regGas,lwd=2,col='blue')
# points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
# segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
# title('Intervalles de pr¨¦diction et donn¨¦es test')

# Visualisation pr¨¦dictions pour la variable Oil

# plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
# abline(regOil,lwd=2,col='blue')
# points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
# segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
# title('Intervalles de pr¨¦diction et donn¨¦es test')

# valeur du score 
# attention au fait que cette valeur peut ¨ºtre superieure ¨¤ 10 car les intervalles de prédiction
# utilisee peuvent conduire ¨¤ une surface sup¨¦rieure ¨¤ 10  

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)
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
print(score_min)
print(min_i)
print(min_j)
#############################################################################
# Am¨¦lioration du mod¨¨le de r¨¦gression Gas ~ Depth
#############################################################################

plot(Gas ~ Depth,xlab="Depth",ylab="Gas")
regGas <- lm(Gas ~ Depth)
abline(regGas,col="red",lwd=2)
title('Gas contre Depth et droite aux moindres carr¨¦s')

# on devine une valeur limite inf¨¦ieure pour la variable ¨¤ pr¨¦dire Gas

Gasinf <- min(Gas)
print(Gasinf)

# il est alors tentant d'essayer une transformation du type log(Gas - seuil) avec seuil ? faire varier (estimer ?)

seuil.G = Gasinf - 0.005     # pour ¨¦viter ln(0) = +infini

logGas <- log(Gas - seuil.G)
plot(logGas ~ Depth,xlab="Depth",ylab="logGas")
reglogGas <- lm(logGas ~ Depth)
abline(reglogGas,col="red",lwd=2)


# C'est d¨¦j¨¤ beaucoup mieux mais on peut encore am¨¦liorer

reglogGas <- lm(logGas ~ Depth + I(Depth^2))
indices <- order(Depth)
lines(Depth[indices],reglogGas$fitted.values[indices],col="blue",lwd=2)
title('logGas contre Depth et r¨¦gression lin¨¦aire multiple')

#############################################################################
# Amélioration du modèle de régression Oil ~ Zone
#############################################################################

plot(Oil ~ Zone,xlab="Zone",ylab="Oil")
regOil <- lm(Oil ~ Zone)
abline(regOil,col="red",lwd=2)
title('Oil contre Zone et droite aux moindres carr¨¦s')

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
title('logOil contre Zone et r¨¦gression lin¨¦aire multiple')

 
# Calcul du score avec le modèle reglogGas pour prédire Gas et reglogOil pour prédire Oil


Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevlogGas <- predict(reglogGas,new=Depth.new,interval="pred",level=0.7)
# attention ? bien utiliser la tranformation inverse
ICprevGas <- exp(ICprevlogGas) + seuil.G
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevlogOil <- predict(reglogOil,new=Zone.new,interval="pred",level=0.7)
ICprevOil <- exp(ICprevlogOil) + seuil.O

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)


# Visualisation prédictions pour la variable Gas

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
title('Intervalles de pr¨¦diction et donn¨¦es test')

# Visualisation prédictions pour la variable Oil

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
title('Intervalles de pr¨¦diction et donn¨¦es test')





