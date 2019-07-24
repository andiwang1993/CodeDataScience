library("rgl", lib.loc="D:/R/R-3.4.2/library")
###############1.1############
n<-200000
X<-rnorm(n,0,1)
Y<-rnorm(n,0,1)
Z<-rnorm(n,0,1)

x<-do.call(cbind,list(X,Y,Z))
V<-matrix(0,n,1)
v<-matrix(0,n,3)
for(i in 1:n){
  #V[i]<-norm(t(x[i,]))
  V[i]<-sqrt(x[i,1]^2+x[i,2]^2+x[i,3]^2)
  for (j in 1:3){
    v[i,j]<-x[i,j]/V[i]
  }
}
open3d()
plot3d(v[,1],v[,2],v[,3],col=rainbow(n))
plot3d(x[,1],x[,2],x[,3],col=rainbow(n))

varmat<-var(x)###var->I3 matrice covariance-variance
cormat<-cor(x)###cor->I3
g1<- 1/n*do.call(cbind,list(sum(X),sum(Y),sum(Z)))##centre de gravite du nuage

s1<-0
for (i in 1:n){
  s1<-s1+sum((x[i,]-g1)^2)
}
  
It1<-1/n*s##inertie totale du nuage
eig<-eigen(varmat)
valeurp<-eig$values ##valeur propre
vectp<-eig$vectors ##vecteur propre

##acp1<-dudi.pca(x)
##interL<-inertia.dudi(acp1,row.inertia = TRUE)
##qlt1<-interL$row.rel/100



################1.2###################
X2<-sort(rnorm(1000))
Y2<-rnorm(1000)
Z2<-rnorm(1000)+atan2(X2,Y2)
x2<-do.call(cbind,list(X2,Y2,Z2))
V2<-matrix(0,n,1)
v2<-matrix(0,n,3)
for(i in 1:n){
  #V[i]<-norm(t(x[i,]))
  V2[i]<-sqrt(x2[i,1]^2+x2[i,2]^2+x2[i,3]^2)
  for (j in 1:3){
    v2[i,j]<-x2[i,j]/V2[i]
  }
}
open3d()
plot3d(v2[,1],v2[,2],v2[,3],col=rainbow(n))
plot3d(x2[,1],x2[,2],x2[,3],col=rainbow(n))

varmat2<-var(x2)###var->I3
cormat2<-cor(x2)###cor->I3
g2<- 1/n*do.call(cbind,list(sum(X2),sum(Y2),sum(Z2)))##centre de gravite du nuage

s2<-0
for (i in 1:n){
  s2<-s2+sum((x2[i,]-g2)^2)
}

It2<-1/n*s2##inertie totale du nuage
eig2<-eigen(varmat2)
valeurp2<-eig2$values ##valeur propre
vectp2<-eig2$vectors ##vecteur propre


#############partie2##################
##(1)
grp1<-c(52,10,40,104,50,27,146,31,46)
grp2<-c(94,38,23,197,99,16,141)
echan<-matrix(0,1,16)
echan[,1:9]<-grp1

echan[,10:16]<-grp2
m1<-mean(grp1)
medi1<-median(grp1)
m2<-mean(grp2)
medi2<-median(grp2)
m<-mean(echan)
medi<-median(echan)

sd1<-sd(grp1)
sd2<-sd(grp2)
sd<-sd(echan)
se1<-sd1/sqrt(9)
se2<-sd2/sqrt(7)
se<-sd/sqrt(16)

##(2)
erreur_standard <- function(nb_echantillons){
  
  SE1_MOY <- matrix(data=rep(x = 0), nrow = 1, ncol = length(nb_echantillons))
  SE2_MOY <- matrix(data=rep(x = 0), nrow = 1, ncol = length(nb_echantillons))
  
  SE1_MED <- matrix(data=rep(x = 0), nrow = 1, ncol = length(nb_echantillons))
  SE2_MED <- matrix(data=rep(x = 0), nrow = 1, ncol = length(nb_echantillons))
  
  for (j in 1:length(nb_echantillons)){
    
    Bp <- matrix(data=rep(x = 0), nrow = nb_echantillons[j], ncol = length(grp1))
    Bt <- matrix(data=rep(x = 0), nrow = nb_echantillons[j], ncol = length(grp2))
    
    MOY1 <- matrix(data=rep(x = 0), nrow = 1, ncol = nb_echantillons[j])
    MOY2 <- matrix(data=rep(x = 0), nrow = 1, ncol = nb_echantillons[j])
    MU1 <- matrix(data=rep(x = 0), nrow = 1, ncol = nb_echantillons[j])
    MU2 <- matrix(data=rep(x = 0), nrow = 1, ncol = nb_echantillons[j])
    
    for (i in 1:nb_echantillons[j]) {
      
      Bp[i,] <- sample(grp1, length(grp1),replace=TRUE)
      MOY1[i] <- mean(Bp[i,])
      MU1[i] <- median(Bp[i,])
      Bt[i,] <- sample(grp2, length(grp2), replace=TRUE)
      MOY2[i] <- mean(Bt[i,])
      MU2[i] <- median(Bt[i,])
    } 
    
    SE1_MOY[j] <- sd(MOY1)/sqrt(nb_echantillons[j]-1)
    SE2_MOY[j] <- sd(MOY2)/sqrt(nb_echantillons[j]-1)
    
    SE1_MED[j] <- sd(MU1)/sqrt(nb_echantillons[j]-1)
    SE2_MED[j] <- sd(MU2)/sqrt(nb_echantillons[j]-1)
  }
  x <- cbind(t(SE1_MOY),t(SE2_MOY),t(SE1_MED),t(SE2_MED))
  return(x)
}



nb_echantillons <- seq(50, 5000,  by=50) 
erreur_standard(nb_echantillons)


plot(x = nb_echantillons, y = erreur_standard(nb_echantillons)[,1], col=rainbow(length(nb_echantillons)))
plot(x = nb_echantillons, y = erreur_standard(nb_echantillons)[,2], col=rainbow(length(nb_echantillons)))
plot(x = nb_echantillons, y = erreur_standard(nb_echantillons)[,3], col=rainbow(length(nb_echantillons)))
plot(x = nb_echantillons, y =erreur_standard(nb_echantillons)[,4], col=rainbow(length(nb_echantillons)))

##


points<-matrix(0,100,2)
for (B in 1:100){
  erreur_standard(B)
  points[B,]<-quantile(x,c(0.025,0.975))##pour
}

