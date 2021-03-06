# TP 2 : Pr��vision volume de bois
# Majeure Data Science 2017-2018

############################################################
# chargement des donn�es et visualisation
############################################################

arbres <- read.table("arbres.txt",header=T)
print(names(arbres))
ht <- arbres$hauteur      # variable hauteur
circ <- arbres$circonf    # variable circonf�rence
plot(ht ~ circ,xlab="circonference (cm)",ylab="hauteur (m)")

############################################################
# m��thode naive
# volume moyen observ�� et volume total de bois
############################################################

vol <-  ((0.01*circ)^2)*(ht^3)/(12*pi*(ht - 1.30)^2)
vol.mean <- mean(vol)
nb.arbres <- 100000
vol.naif <- nb.arbres*vol.mean
print(round(vol.naif,digits=0))

# volume �� 95% par approximation normale
vol.sd <- sd(vol)
vol.naif.crit <- vol.naif - qnorm(0.95)*sqrt(nb.arbres)*vol.sd  
print(round(vol.naif.crit,digits=0))

############################################################
###  Un mod��le de r��gression lin��aire simple
############################################################
####reg <- lm(ht ~ circ)
reg <- lm(ht ~ circ+circ*circ)
abline(reg,col="red")
reg.s <- summary(reg)
print(reg.s)

# estimation + pr��diction : pas utile pour la suite mais permet de juger graphiquement
# de la qualit�� du mod��le en termes d'estimation et de pr��diction

plot(ht ~ circ,pch="+",col="grey60")
circnew <- seq(min(circ),max(circ),length=100)
grille <- data.frame(circ=circnew)
ICdte <- predict(reg,new=grille,interval="conf",level=0.95)
ICprev <- predict(reg,new=grille,interval="pred",level=0.95)
matlines(circnew,cbind(ICdte,ICprev[,-1]),lty=c(1,2,2,3,3),col=c(1,2,2,3,3))

# pr��vision de volume par m��thode Monte-Carlo 

# simulation d'une for��t
beta0 <- reg$coef[1]
beta1 <- reg$coef[2]
sigma <- reg.s$sigma    #estimation �cart-type des r�sidus
print(reg.s)

# fonction R pour simuler une for�t de nb.arbres d�crits par le couple (circonf�rence,hauteur)
# avec circonf�rence en cm calcul�e ? 1m30 du sol et hauteur en m
# utilise le mod�le de r�gression + la loi normale N(mean=47,sd=8.5) pour la variable circonf�rence

foret.simu <- function(nb.arbres) {
  circ.simu <- rnorm(nb.arbres,mean=47,sd=8.5)
  ht.simu <- beta0 + beta1*circ.simu + sigma*rnorm(nb.arbres,mean=0,sd=1)
  foret <- data.frame(circ=circ.simu,ht=ht.simu)
  return(foret)
}

# fonction R pour calculer le volume total de bois d'une for�t

volume.foret <- function(foret) {
  circ.f <- 0.01*foret$circ              # circonf�rence en m�tre
  ht.f <- foret$ht
  vol.f <- (circ.f^2)*(ht.f^3)/((ht.f-1.30)^2)
  return(sum(vol.f)/(12*pi))
}

# simulations Monte-Carlo de la variable volume total de bois d'une for�t de 100 000 arbres
# n�cessite quelques secondes de calcul!

nb.arbres <- 100000
N <- 400            # nombre de simulations Monte-Carlo
vol.rls <- rep(0,N) # initialisation vol.rls = volume simul? ? l'aide d'une r�gression lin�aire simple (rls)
for (k in (1:N)) {vol.rls[k] <- volume.foret(foret.simu(nb.arbres))}

# calcul du quantile �� 5% (vol.crit ou volume critique) et analyse de la convergence Monte-Carlo

plot(vol.rls,xlab="num��ro de simulation",ylab="Volume total de bois (m3)")
vol.crit <- quantile(vol.rls,probs=0.05)
print(round(vol.crit,digits=0))
abline(h=vol.crit,col="red",lwd=2)
# analyse convergence pour savoir si N est assez grand
volcrit <- rep(0,N)
for (k in 1:N) {
  volcrit[k] <- quantile(vol.rls[1:k],probs=0.05)
}
lines(1:N,volcrit,col="blue",lwd=2)
title(main=list("Volume total de bois ? 95%",cex=1))
 
####################################################################
# Prise en compte de l'incertitude sur les param��tres du mod��le
####################################################################

print(reg.s)
names(reg.s)
sigma.hat <- reg.s$sigma    # estimation sigma des r��sidus
beta.hat <- reg$coef        # estimation des coefficients beta
Cov.beta <- vcov(reg)       # matrice de var-covariance des coefs beta

# fonction R de simulation d'une for��t en fonction de la valeur des coefficients de r��gression

foret.simu.u <- function(nb.arbres,beta,sig) {
  #circ.simu <- pmin(pmax(rnorm(nb.arbres,mean=47,sd=8.5),20),80)
  circ.simu <- rnorm(nb.arbres,mean=47,sd=8.5)
  ht.simu <- beta[1] + beta[2]*circ.simu + sig*rnorm(nb.arbres,mean=0,sd=1)
  foret <- data.frame(circ=circ.simu,ht=ht.simu)
  return(foret)
}

# simulations Monte-Carlo de la variable volume total de bois d'une for�t de 100 000 arbres
# avec prise en compte de l'incertitude sur les param��tres de la r��gression
# n��cessite ��galement quelques secondes de calcul!

library("MASS")
nb.arbres <- 100000
N <- 400
vol.u <- rep(0,N)
for (k in (1:N)) {
  beta <- as.vector(beta.hat + mvrnorm(n=1,mu=rep(0,2), Sigma=Cov.beta)) # Sigma d�signe ici la matrice des var-covariances
  sig <- sigma.hat*sqrt(rchisq(n=1,df=98)/98)
  vol.u[k] <- volume.foret(foret.simu.u(nb.arbres,beta,sig))
}

# visualisation et comparaison 

ylim <- c(min(vol.u)-10,max(vol.u)+10)
plot(vol.rls,xlab="num�ro de simulation",ylab="Volume total de bois (m3)",ylim = ylim)
abline(h=vol.crit,col="black",lwd=2)
points(vol.u,col="red")
vol.crit.u <- quantile(vol.u,probs=0.05)
abline(h=vol.crit.u,col="red",lwd=2)
print(round(vol.crit.u,digits=0))
title(main=list("Comparaison pour le calcul du volume total de bois ? 95%",cex=1))

####################################################################
# Validation du mod��le de r��gression : hauteur ~ circonf��rence
# par analyse des r��sidus
####################################################################

# rappel du mod��le de r��gression 
plot(ht ~ circ,data=arbres,xlab="circonf�rence (cm)",ylab="hauteur (m)")
abline(reg,col="red")

# trac? des r�sidus studentis�s contre les r�ponses pr�dites (par le mod�le)
# attention ? bien analyser ce graphique, ne pas trop se presser dans la conclusion!

plot(rstudent(reg) ~ fitted(reg),xlab="R��ponse estim��e",ylab="R��sidus",ylim=c(-3,3))
abline(h=2,col="red")
abline(h=-2,col="red")
title('R��sidus studentis�� contre la r��ponse pr�dite')

# autre outils de validation : droite de Henry pour la normalit�� des r��sidus
plot(reg,which=2)

############################################################
# Un mod��le de r��gression lin��aire multiple
# A compl��ter
############################################################




