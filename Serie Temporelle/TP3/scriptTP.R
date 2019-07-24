# script TP n 3, Series Temporelles, decembre 2017

# Partie 1 :
# On ¨¦tudie la s¨¦rie de trafic a¨¦rien international "airline" (serie Airpassengers de R)
# C'est l'exemple 1 du poly : voir pages 4 et 5

# ********************************************************************************************************
# Partie exploratoire : executer le code pas a pas situe entre deux commentaires signales par # 
# Il n'y a aucune instruction a completer. Par contre, vous pouvez changer certains parametres 
# et visualiser l'effet obtenu
# ********************************************************************************************************

# Chargement des donnees brutes et mise sous forme de serie chronologique avec ts
rm(list=ls())

airlinetab <- read.table("airline.dat")
airline <- airlinetab$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

# Chronogramme et legendes...

plot(airline, type='o', xlab="Annee",
ylab="nombre de passagers (milliers)", main="Trafic a¨¦rien international de janv. 1949 ¨¤ dec. 1960")


# Methodologie de Box&Jenkins, premiere transformation simple par
# Passage au logarithme et visualisation de l'effet obtenu

logair <- log(airline)
op <- par(mfrow = c(1,2))
plot(airline,type='o', main = "S¨¦rie x initiale")
plot(logair, type='o', main = "log(x)")
par(op)

# On elimine la tendance (lineaire) par differenciation simple

difflogair <- diff(logair)
plot(difflogair, type='o', main = "S¨¦rie log(x) differenciee",
xlab="Ann¨¦e", ylab = expression(paste("(",I-B,")",log(x[t])) ))
abline(0, 0, col="red", lwd=2)

# puis differenciation saisonniere (comparer la figure obtenue avec celle du poly page 13)
# pour eliminer la composante periodique de periode s = 12 mois

diff2logair <- diff(difflogair, lag=12)
op <- par(cex.lab = 0.8)
plot(diff2logair, type='o', main = "Diff¨¦renciation simple et diff¨¦renciation saisonni¨¨re",
xlab="Ann¨¦e", ylab = expression(paste("(",I-B^12,")","(",I-B,")",log(x[t]))))
par(op)
abline(0, 0, col="red", lwd=2)

# ********************************************************************************************************
# On deroule la methodologie de Box et Jenkins en partant de la serie qui vient
# d'etre obtenue par deux differenciations successives (serie diff2logair). On l'analyse comme une
# serie stationnaire ? l'aide des ACF et PACF. On verifie alors que le modele SARIMA du poly semble bien 
# adapt? (voir pages 37 et 38 pour l'expression de ce modele). On estime ensuite ce modele, on verifie 
# que les coefficients sont bien ceux du poly et on valide graphiquement. Enfin, on utilise le modele pour 
# faire de la prevision a un an et on visualise la qualite de prevision a un an par une technique de 
# back-testing (voir poly page 45, analyse post-sample)...
# ********************************************************************************************************

# on rebaptise la serie obtenue (sans tendance et desaisonnalisee) 

airdts <- diff2logair

# ACF et PACF de airdts (comparer avec le poly page 38)

op <- par(mfrow = c(1,2))
airdts <- as.vector(airdts)
ro <- acf(airdts , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
lwd=2)
alpha <- pacf(airdts , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)

# ajustement d'un modele SARIMA(0,1,1)(0,1,1) avec saisonnalit? s = 12 et comparaison des coefficients obtenus
# avec ceux du poly (voir page 38 pour l'expression complete du modele et les valeurs des coeff)
# fonction "arima" de R

modele <- arima(logair, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

# validation du modele

# fonction "tsdiag" de R

tsdiag(modele)

# extraction des "residus" du modele (processus de bruit sous-jacent) et "normal qqplot"

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Th¨¦oretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

# Prevision avec horizon h = 12 mois (prevision ? un an de janvier 62 ? decembre 62)
# L'interet ici est de voir comment utliser la fonction "predict" du logiciel R 
# Attention, on prevoit le log de la serie, prendre partout l'exponentielle pour revenir
# ? des prevision sur la serie initiale

logairplot <- ts(c(logair, rep(NA,12)), start = c(1949,1), freq = 12) 
plot(logairplot, type='o', xlab='Annee', ylab='Log du nombre (en milliers) de passagers',
     main ="Pr¨¦vision SARIMA du trafic a¨¦rien sur un an", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)
pred<-prevision
lines( ts(prevision$pred, start=c(1961,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1961,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1961,1), freq=12),  col='blue', lwd=2 )


# Back-testing du modele SARIMA : on enleve les 12 dernieres valeurs que l'on cherche ensuite
# ¨¤ prevoir. On compare alors avec les valeurs reelles de la serie!

nair <- length(airline)
airfit <- airline[1:(nair - 12)]	# on enleve les 12 dernieres valeurs

logairfit <- ts(log(airfit), start = c(1949,1), freq = 12)

# ajustement d'un modele SARIMA(0,1,1)(0,1,1) avec s = 12 sur la serie tronquee logairfit

modele <- arima(logairfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

tsdiag(modele)

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Th¨¦oretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

# prevision avec horizon h = 12

logair <- log(airline)

plot(logair, type='o', xlab='Ann¨¦e', ylab='Log du nombre (en milliers) de passagers',
     main ="Pr¨¦vision SARIMA du trafic aerien et valeurs r¨¦elles", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

# ********************************************************************************************************
#  Travail a faire : le dernier graphique obtenu pour back-tester la methode de prevision porte sur le
# logarithme de la serie. Obtenir la meme analyse graphique mais sur la serie initiale. Pour cela,
# completer le code suivant (les parties a completer sont de la forme ... (3 points de suspension):
# ********************************************************************************************************

modele2<-arima(airfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele2)

tsdiag(modele2)

residus2 <- modele2$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Th¨¦oretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

plot( airline, type='o', xlab='Ann¨¦e', ylab='Nombre (en milliers) de passagers',
     main ="Pr¨¦vision SARIMA du trafic a¨¦rien et valeurs r¨¦elles", ylim=c(100,700))

prevision2 <- predict(modele2, n.ahead=12, prediction.interval=T)

lines( ts(prevision2$pred ,start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( prevision2$pred + 2*prevision2$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( prevision2$pred - 2*prevision2$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )


# ********************************************************************************************************
# On estime de maniere parametrique le tendance et saisonnalit? du log de la serie, et on traite 
# la serie obtenue comme une serie stationnaire. On travaille sur l'historique priv¨¦ des 12 dernieres
# valeurs pour tester ¨¤ nouveau la qualit¨¦ de la prevision obtenue.
# Completer les ...
# ********************************************************************************************************


airlinetab <- read.table("airline.dat")
airline <- airlinetab$V1
airline <- ts(airline, start = c(1949,1), freq = 12)

logair <- log(airline)
nair <- length(logair)

# donnees pour estimer le modele (on enleve les 12 dernieres valeurs)

logairfit <- logair[1:(nair-12)]
logairfit <- ts(logairfit, start=c(1949,1), freq=12)

plot(logairfit, type='o', xlab="Annee",
ylab="Log du nombre de passagers (milliers)", main="Trafic a¨¦rien international de janv. 1949 ¨¤ dec. 1959")

# on estime tendance et saisonnalit? ? l'aide d'un modele lineaire

logairfit <- as.vector(logairfit)

# predicteurs avec predic1 qui est la variable temps

predic1 <- 1:(nair-12)

predic2<-logairfit-c(rep(NA,1),logairfit[1:131])#+0.6*logairfit+0.2*c(logairfit[2:132],rep(NA,1))
# predic2<-na.omit(predic2)
predic3 <- rnorm(nair-12)

# On utise la fonction lm de R pour estimer le modele lineaire

mod.lm <- lm( logairfit ~ predic1 + predic2 + predic3 )

# on en deduit la tendance 

tend <- mod.lm$coef[1] + mod.lm$coef[2]*predic1
tend <- ts(tend, start=c(1949,1), freq=12)
lines(tend, col='red',lwd=2)

# on calcule la saisonnalit? 

sais <- mod.lm$coef[3]*predic2 + mod.lm$coef[4]*predic3
sais <- ts(sais, start=c(1949,1), freq=12)
lines(tend+sais, col='blue',lwd=2)
plot(sais,type = 'o')
# serie log(airline) sans tendance et saisonnalit?

logairdts <- logairfit - (tend + sais)
logairdts <- ts(logairdts, start=c(1949,1), freq=12)
op <- par(cex.lab = 0.8)
plot(logairdts, type='o', main = "Residus estimes avec le modele lineaire", xlab="Annee")
par(op)
abline(0, 0, col="red", lwd=2)

# ACF et PACF de logairdts 

op <- par(mfrow = c(1,2))
logairdts <- as.vector(logairdts)
ro <- acf(na.omit(logairdts) , lag=25, ylim = c(-1,1), main = expression("ACF serie stationnarisee"), xlab="Lag (en mois)", lwd=2)
alpha <- pacf(na.omit(logairdts) , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


# ajustement d'un modele SARIMA 

logairdts <- ts(logairdts, start=c(1949,1), freq=12)
logairdtsdiff6 <- diff(logairdts, lag=6)

op <- par(mfrow = c(1,2))
logairdtsdiff6 <- as.vector(logairdtsdiff6)
ro <- acf(na.omit(logairdtsdiff6) , lag=25, ylim = c(-1,1), main = expression("ACF serie stationnarisee"), xlab="Lag (en mois)", lwd=2)
alpha <- pacf(na.omit(logairdtsdiff6) , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


modele3 <- arima(logairdts, order = c(0,1,1), seasonal = list(order=c(0,1,2), period = 12))
print(modele3)

tsdiag(modele3)

residus <- modele3$residuals
qqnorm(as.vector(residus), main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")
# prevision de logairdts avec horizon h = 12 

plot(logair, type='o', xlab='Annee', ylab='Log du nombre (en milliers) de passagers',
     main ="Prevision Modele lineaire + SARIMA", ylim=c(4.5,6.8))

prevision3 <- predict(modele3, n.ahead=12, prediction.interval=T)

# attention ? tenir compte de la tendance et saisonnalit?

prevlm <- as.vector(prevision3$pred)
pred1 <- (nair-11):nair
pred2 <- logair[133:144]-logair[121:132]
pred3 <- rnorm(12)
tend <- mod.lm$coef[1] + mod.lm$coef[2]*pred1
sais <- mod.lm$coef[3]*pred2 + mod.lm$coef[4]*pred3

prevlm <- prevlm + tend + sais

lines( ts(prevlm, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevlm + 2*prevision3$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevlm - 2*prevision3$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

#s¨¦rie initiale

modele6<-arima(ts(airline[1:(nair-12)], start = c(1949,1), freq = 12), order = c(0,1,1), seasonal = list(order=c(0,1,2), period = 12))
pred6<-predict(modele6,n.ahead = 12,prediction.interval=T)
plot(airline,type='o')
lines( ts(pred6$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(pred6$pred + 2*pred6$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(pred6$pred - 2*pred6$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
######################################################################################################
# Partie 2 : on s'interesse ? l'evolution de la temperature annuelle moyenne en France depuis 1900 
# jusqu'? 2015 ? travers l'ecart enregistr? par rapport ? la periode de reference 1961-1990
# Cette partie est relativement libre mais on s'interessera naturellement ? la question  
# du rechauffement en France ces dernieres decennies...
######################################################################################################
par(op)
data  <- read.table("ecart.txt",header=TRUE)
names(data)

diffdat<-diff(as.vector(data$ecart))
diffdat<-ts(diffdat,start = c(1901,1),freq=1)
diff2dat<-diff(diffdat)
plot(data,type='o',main = "S¨¦rie x initiale")
plot(diffdat, type='o', main = "diff(x)")
plot(diff2dat,type='o')
abline(0,0,col="red", lwd=2)

op <- par(mfrow = c(1,2))
datts <- as.vector(data)
ro <- acf(datts[2] , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(datts[2] , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


op <- par(mfrow = c(1,2))
datdts <- as.vector(diffdat)
ro <- acf(datdts , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(datdts , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)

op <- par(mfrow = c(1,2))
diff2dat <- as.vector(diff2dat)
ro <- acf(diff2dat , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(diff2dat , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)


#############partie3############
library(datasets)
dat3<-UKDriverDeaths
plot(dat3)
op <- par(mfrow = c(1,2))
dat3<-as.vector(dat3)
ro <- acf(dat3 , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(dat3 , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)
uk<-log(dat3)
uk<-ts(uk,start=c(1969,1),freq=12)
plot(uk,type='o')

op <- par(mfrow = c(1,2))

ro <- acf(as.vector(uk) , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(as.vector(uk), lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)
nb3<-length(uk)

train<-uk[1:(nb3-12)]
train<-ts(train,start=c(1969,1),freq=12)

modele4<-arima(train,order = c(1,0,0),seasonal = c(1,0,0))
print(modele4)

tsdiag(modele4)

residus <- modele4$residuals
qqnorm(as.vector(residus), main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")
# prevision de logairdts avec horizon h = 12 
# plot(modele4$residus)
pred4<-predict(modele4,n.ahead=12,prediction.interval=T)
plot(uk,type='o',ylim=c(6.8,8))
lines( ts(pred4$pred, start=c(1984,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(pred4$pred + 2*pred4$se, start=c(1984,1), freq=12),  col='blue', lwd=2 )
lines( ts(pred4$pred - 2*pred4$se, start=c(1984,1), freq=12),  col='blue', lwd=2 )

######partie 3)choix personnel##########
dat4<-USAccDeaths
plot(dat4)
dat4<-log(dat4)
op <- par(mfrow = c(1,2))

ro <- acf(as.vector(dat4) , lag=25, ylim = c(-1,1), main = expression("ACF s¨¦rie stationnaris¨¦e"), xlab="Lag (en mois)",
          lwd=2)
alpha <- pacf(as.vector(dat4), lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)
nb4<-length(dat4)
train<-dat4[1:(nb4-12)]
train<-ts(train,start=c(1973,1),freq=12)

modele5<-arima(train,order = c(0,1,1),seasonal = list(order=c(0,1,1),period=12))

tsdiag(modele5)

residus <- modele5$residuals
qqnorm(as.vector(residus), main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

plot(dat4,type='o',ylim=c(8.7,9.5))
pred5<-predict(modele5,n.ahead=12,prediction.interval=T)
lines( ts(pred5$pred, start=c(1978,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(pred5$pred + 2*pred5$se, start=c(1978,1), freq=12),  col='blue', lwd=2 )
lines( ts(pred5$pred - 2*pred5$se, start=c(1978,1), freq=12),  col='blue', lwd=2 )

