# Script du TP n?2, Series Temporelles, Majeure Science des Donnees 2017-18

##############################################################################
# PARTIE 1 : Etude d'un AR(2)
# Modele X(t) = mu + phi1*(X(t-1)-mu) + phi2*(X(t-2)-mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)
# simulation d''un AR(2) par une phase initiale de stationnarisation
##############################################################################

# inverses des racines du polynome P(z) = 1 - phi1*z - phi2*Z^2

# cas de deux racines reelles dans ]-1, 1[
# r1 <- 0.9
# r2 <- -0.9
# phi1 <- r1 + r2  
# phi2 <- - r1*r2			# parametres AR(2) 

# cas de deux racines complexes conjuguees de module r < 1
r <- 0.9
angle <- 60 			# en degres dans [0, 180]
phi1 <- 2*r*cos(angle*pi/180)
phi2 <- - r*r			# parametres AR(2)

mu <- 0			        # moyenne du processus X[t]
sigZ <- 1	                # ecart-type du bruit Z[t]

# simulation avec regime transient de taille ninit = 50
ninit <- 50
n <- 200
ntot <- ninit + n

xtot <- rep(0,ntot)
xtot[1] <- 0
xtot[2] <- 0

for (t in 3:ntot) xtot[t] <- phi1*xtot[t-1] + phi2*xtot[t-2] + sigZ*rnorm(1)

xtot <- mu + xtot             # decentrage
xinit <- xtot[1:ninit]        # regime transient (initial)

xstat <- xtot[(ninit+1):ntot] # regime stationnaire --> AR(2) de taille n

# visualisation regime transient
plot(xtot, type='o', xlab="Temps t", main = "AR(2) simul? avec regime transient", col="grey")
lines((ninit+1):ntot, xstat, type='o')
abline(mu, 0, col="red", lwd=2)

# analyse graphique - chronogramme de la serie xstat  

plot(xstat,type='o',xlab='Temps t',main = "Simulation d'un AR(2)")
abline(mu,0,col='red')

# acf et pacf de la serie simulee
op <- par(mfrow = c(1,2))
ro <- acf(xstat, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(xstat, lag=15, ylim = c(-1,1), main = "et PACF", xlim=c(0,15))
par(op)


###############################################################################
# PARTIE 2 : identification de modeles
############################################################################### 

# On commence avec la premiere serie de donnees, fichier "serie1.Rdata"

rm(list=ls())           # clear all 
load("serie3.Rdata")
ls.str()

# chronogramme de la serie  
plot(serie, type='o', xlab="Temps t", ylab="", main = "data")
abline(h=0, col="red", lwd=2)


# acf et pacf de la serie  
op <- par(mfrow = c(1,2))
ro <- acf(serie, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(serie, lag=15, ylim = c(-1,1), main = "et PACF empirique", xlim=c(0,15))
par(op)

# library(tseries)
# beta<-arma(serie,order=c(2,0))
# plot(beta)
# beta
# summary(beta)

library(stats)

beta<-arima(serie,order = c(0,5,1)) ##variables p,d,q
beta
# preserie<-predict(beta,newdata=serie)
# ro <- acf(preserie, lag=15, ylim = c(-1,1), main = "ACF empirique")
# alpha <- pacf(preserie, lag=15, ylim = c(-1,1), main = "et PACF empirique", xlim=c(0,15))
