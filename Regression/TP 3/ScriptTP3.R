# TP3 : Régression Linéaire pour le challenge Total
# Majeure Data Science 2017-2018

# Sélection de la variable la plus corrélée avec chacune des deux variables
# ? prédire et régression linéaire simple (rls : p = 1 prédicteur seulement)

# chargement des données et nettoyage
data <- read.csv2('TrainSample.csv')
nums <- sapply(data, is.numeric) 
data <- data[ , nums] # variables numériques uniquement
data$API <- NULL       # on enl¨¨ve l'identifiant

# les deux variables ¨¤ pr¨¦dire (deux derni¨¨res variables en colonne de data)
Gas <- data[,42]
Oil <- data[,43]

# data test et training data 

ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- data[ind.test,]
data.train <- data[-ind.test,]


# sélection de la variable la plus corrélée avec Gas
C <- cor(data,use="complete.obs")
indPredGas <- which.max(abs(C[1:41,42]))
names(data)[indPredGas]

# sélection de la variable la plus corrélée avec Oil
C <- cor(data,use="complete.obs")
indPredOil <- which.max(abs(C[1:41,43]))
names(data)[indPredOil]


# jeux de données d'apprentissage correspondant
# pour prédire Gas ? partir d'une rls
dataGas <- data.frame(Depth = data.train[, indPredGas], Gas = data.train[, 42])
# pour prédire Oil ? partir d'une rls
dataOil <- data.frame(Zone = data.train[, indPredOil], Oil = data.train[, 43])



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

# calcul des intervalles de prédiction pour Gas et Oil

Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevGas <- predict(regGas,new=Depth.new,interval="pred",level=0.95)
Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevOil <- predict(regOil,new=Zone.new,interval="pred",level=0.95)

# Visualisation pr¨¦dictions pour la variable Gas

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
abline(regGas,lwd=2,col='blue')
points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
segments(x0=data.test[,indPredGas],y0 = ICprevGas[,2],y1 = ICprevGas[,3],col='blue')
title('Intervalles de prédiction et données test')

# Visualisation pr¨¦dictions pour la variable Oil

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
abline(regOil,lwd=2,col='blue')
points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
segments(x0=data.test[,indPredOil],y0 = ICprevOil[,2],y1 = ICprevOil[,3],col='blue')
title('Intervalles de prédiction et données test')

# valeur du score 
# attention au fait que cette valeur peut être supérieure ? 10 car les intervalles de prédiction
# utilisés peuvent conduire ? une surface supérieure ? 10  

ktest <- 100
score <- score.fun(ktest,ICprevGas,ICprevOil,data.test)

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


# on peut encore am¨¦liorer

reglogOil <- lm(logOil ~ Zone + I(Zone^2))
indices <- order(Zone)
lines(Zone[indices],reglogOil$fitted.values[indices],col="blue",lwd=2)
title('logOil contre Zone et régression linéaire multiple')

 
# Calcul du score avec le mod¨¨le reglogGas pour pr¨¦dire Gas et reglogOil pour pr¨¦dire Oil
# A compl¨¦ter...



# Visualisation pr¨¦dictions pour la variable Gas
# A compl¨¦ter...



# Visualisation pr¨¦dictions pour la variable Oil
# A compl¨¦ter...


####################################

# calcul des intervalles de prédiction pour Gas et Oil

#Depth.new <- data.frame(Depth=data.test[,indPredGas])
ICprevGas2 <- predict(reglogGas,new=Depth.new,interval="pred",level=0.95)
#Zone.new <- data.frame(Zone=data.test[,indPredOil])
ICprevOil2 <- predict(reglogOil,new=Zone.new,interval="pred",level=0.95)

# Visualisation pr¨¦dictions pour la variable Gas

plot(Gas ~ Depth,pch="+",col="grey60",ylim=c(-3,6))
abline(reglogGas,lwd=2,col='blue')
points(data.test[,42] ~ data.test[,indPredGas],pch="+",col='red')
segments(x0=data.test[,indPredGas],y0 = ICprevGas2[,2],y1 = ICprevGas2[,3],col='blue')
title('Intervalles de prédiction et données test')

# Visualisation pr¨¦dictions pour la variable Oil

plot(Oil ~ Zone,pch="+",col="grey60",ylim=c(-3,6))
abline(reglogOil,lwd=2,col='blue')
points(data.test[,43] ~ data.test[,indPredOil],pch="+",col='red')
segments(x0=data.test[,indPredOil],y0 = ICprevOil2[,2],y1 = ICprevOil2[,3],col='blue')
title('Intervalles de prédiction et données test')

# valeur du score 
# attention au fait que cette valeur peut être supérieure ? 10 car les intervalles de prédiction
# utilisés peuvent conduire ? une surface supérieure ? 10  

ktest <- 100
###scoresimple<- score.fun(ktest,dataGas,dataOil,data.test)
score2.fun <- function(ktest,ICprevGas2,ICprevOil2,data.test) {
  score2 <- 0
  for (k in 1:ktest) {
    if ( (ICprevGas2[k,"lwr"] > data.test[k,42]) | (ICprevGas2[k,"upr"] < data.test[k,42]) |
         (ICprevOil2[k,"lwr"] > data.test[k,43]) | (ICprevOil2[k,"upr"] < data.test[k,43]) )  
      score2 <- score2 + 10
    else
      score2 <- score2 + (ICprevGas2[k,"upr"]-ICprevGas2[k,"lwr"])*(ICprevOil2[k,"upr"]-ICprevOil2[k,"lwr"])
  }
  score2 <- score2/ktest
  return(score2)
}
score2 <- score2.fun(ktest,ICprevGas2,ICprevOil2,data.test)
