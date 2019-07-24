# R¨¦gression lin¨¦aire sous R
# Majeure Data Science 2017-2018

# Chargement des donn¨¦es de pluie/rendement, cf. polycopi¨¦ du cours "Introduction ¨¤ la R¨¦gression"

data1 <- read.table("data1.txt", header=TRUE)

is.data.frame(data1)
names(data1)
print(data1)  
summary(data1)


# r¨¦gression lin¨¦aire simple de la variable "rendement" sur la variable "pluie" avec la fonction lm()

pluie <- data1$pluie
rend <- data1$rendement
data1.reg <- lm(rend ~ pluie)
print(data1.reg)
    
# repr¨¦sentation graphique

plot(pluie,rend,xlab="hauteur pr¨¦cipitations cumul¨¦es (m)",ylab="rendement (sans unit¨¦)",lwd=2)
abline(data1.reg,lwd=2,lty=1,col="red")
title(main=list("R¨¦gression lin¨¦aire : rendement de bl¨¦ contre quantit¨¦ de pluie",cex=1))

# Information suppl¨¦mentaire avec summary

data1.reg.s <- summary(data1.reg)
print(data1.reg.s)

# Table d'analyse de la variance (ANOVA)
anova(data1.reg)

# intervalles de confiance sur les param¨¨tres
confint(data1.reg)
confint(data1.reg,level=0.9)

# ellipse de confiance
library(car) # pour tracer des ellipses
confidenceEllipse(data1.reg)

# Matrice de var-covar des param¨¦tres estim¨¦s
VarBeta.hat <- vcov(data1.reg)
print(VarBeta.hat)

# intervalles de confiance pour la r¨¦ponse esp¨¦r¨¦e

plot(pluie,rend,xlab="hauteur pr¨¦cipitations cumul¨¦es (m)",ylab="rendement (sans unit?)",lwd=2,pch=3)
abline(data1.reg,lwd=2,lty=1,col="blue")

newdata <- data.frame(pluie=seq(min(pluie),max(pluie),by=0.01)) # attention ¨¤ bien sp¨¦cifier pluie = ...
int.conf <- predict(data1.reg,newdata,interval="confidence") 
lines(newdata[,1],int.conf[,2],col="red",lty=2,lwd=2)
lines(newdata[,1],int.conf[,3],col="red",lty=2,lwd=2)


# Intervalle de pr¨¦ciction pour la r¨¦ponse

int.pred <- predict(data1.reg,newdata,interval="prediction") 
lines(newdata[,1],int.pred[,2],col="black",lty=2)
lines(newdata[,1],int.pred[,3],col="black",lty=2)
title(main=list("Intervalle de confiance pour la r¨¦ponse esp¨¦r¨¦e et intervalle de pr¨¦diction",cex=0.8))

#analyse des r¨¦sidus

res1 <- residuals(data1.reg)
n <- length(res1)
plot(1:n,res1,ylim=c(-0.2,0.15),xlab="num¨¦ro d'observation i = 1, ..., n ",ylab="r¨¦sidus estim¨¦s")
abline(h=0,lty=2)
title(main=list("Trace s¨¦quentiel des r¨¦sidus estim¨¦s",cex=1))

plot(data1.reg$fitted.values,res1,ylim=c(-0.2,0.15),xlab="r¨¦ponse estim¨¦e",ylab="r¨¦sidus estim¨¦s")
abline(h=0,lty=2)
title(main=list("Trace des r¨¦sidus estim¨¦s en fonction de la r¨¦ponse estim¨¦e",cex=1))

plot(pluie,res1,ylim=c(-0.2,0.15),xlab="r¨¦ponse estim¨¦e",ylab="r¨¦sidus estim¨¦s")
title(main=list("Trace des r¨¦sidus estim¨¦s en fonction de la variable pluie",cex=1))

# vers le mod¨¨le lin¨¦aire y ~ x + x^2 
pluie2 <- pluie*pluie
data1.reg2 <- lm(rend ~ pluie + pluie2, data=data1)
print(data1.reg2)

plot(pluie,rend,xlab="hauteur pr¨¦cipitations cumul¨¦es (m)",ylab="rendement (sans unit?)",lwd=2,xlim=c(0,0.5),ylim=c(0,0.9))
lines(pluie,data1.reg2$fitted.values,col="blue",lwd=2)
title(main=list("R¨¦gression lin¨¦aire avec terme quadratique",cex=1))

res2 <- residuals(data1.reg2)
plot(data1.reg2$fitted.values,res2,xlim=c(0,1),ylim=c(-0.15,0.1),xlab="r¨¦ponse estim¨¦e",ylab="r¨¦sidus estim¨¦s")
abline(h=0,lty=2)
title(main=list("Trac¨¦ des r¨¦sidus estim¨¦s en fonction de la r¨¦ponse estim¨¦e",cex=1))

# V¨¦rification de la d¨¦pendance des r¨¦sidus : ¨¤ compl¨¦ter
t1<-sum(res2)
t2<-sum(res2*pluie)
t3<-sum(res2*pluie2)


#r¨¦sidus standardis¨¦s
plot(rstandard(data1.reg2),xlab="num¨¦ro d'observation i",ylab="R¨¦sidus",ylim=c(-3,3),lwd=2)
abline(h=0,lty=2)


#r¨¦sidus studentis¨¦s
points(rstudent(data1.reg2),col="red",pch=3)
abline(h=qt(p=0.025,df=data1.reg2$df.residual),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=data1.reg2$df.residual),col="red",lwd=2,lty=2)
title('R¨¦sidus standardis¨¦s et studentis¨¦s')

# partie II 1

load("dataTotal.RData")
names(dataChallenge)

GasCum <- dataChallenge$GasCum
Depth <- dataChallenge$Depth
Zone <- dataChallenge$Zone
Temperature <- dataChallenge$Temperature

pairs(dataChallenge)

# r¨¦gression de la variable d'intérêt GasCum sur les autres variables : ¨¤ compl¨¦ter

# partie II 2

data2 <- read.table("data2.txt", header=TRUE)
names(data2)
nb <- data2$nb
dist <- data2$dist
temps <- data2$temps

pairs(data2)

# r¨¦gression de la variable d'intérêt temp sur les autres variables : ¨¤ compl¨¦ter






