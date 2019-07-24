#Volcan etude du cas
rm(list=ls())
source("mainScript_DatascienceClass.R")
#Tessellations centroidales de Voronoi
generateCVT <- function(npts, dim, nite){
  J<-matrix(1,npts,1)
  X<-matrix(0.5,nrow=npts, ncol=dim)
  for(i in 1:nite){
    w<-runif(dim,min=0,max=1)
    d<-matrix(0,npts,1)
    for(j in 1:npts){
      d[j]<-sqrt(sum((w-X[j,])^2))
    }
    k<-which(d==min(d))
    k1<-k[1]
    X[k1,]<-(J[k1]*X[k1,]+w)/(J[k1]+1)
    J[k1]<-J[k1]+1
  }
  
  return(X)
}

generateLHS <- function(npts, dim){
  return(replicate(dim,sample.int(n=npts)/npts))
}

#critere

evalMinDist <- function(X){ 
  D<-dist(X)
  D<-as.matrix(D)
  npts<-length(X[,1])
  d<-min(D+diag(npts))
  return( list(minDist = d, allDist = D) ) 
}


OptimLHS<-function(npts,dim,nite){
  
  dopti<-0
  Xopti<-matrix(0,npts,dim)
  Dopti<-as.numeric()
  for(i in 1:nite){
    X<-generateLHS(npts,dim)
    d<-evalMinDist(X)$minDist
    D<-evalMinDist(X)$allDist
    if(d>dopti){
      Xopti<-X
      dopti<-d
      Dopti<-D
    }
  }
  return(list(Xoptim=Xopti,dOptim=dopti,Dall=Dopti))
  
}

OptimLHS1<-function(npts,dim,nite){
  
  X<-generateLHS(npts,dim)
  Xnew<-X
  d<-evalMinDist(X)$minDist
  D<-evalMinDist(X)$Dall
  for(i in 1:nite){
    #calculer nouveau X et nouveau d
    dimtest<-sample(1:dim,1)
    nptest<-sample(1:npts,2)
    Xnew[nptest[1],dimtest]<-X[nptest[2],dimtest]
    Xnew[nptest[2],dimtest]<-X[nptest[1],dimtest]
    dnew<-evalMinDist(Xnew)$minDist
    Dnew<-evalMinDist(Xnew)$Dall
    if(dnew>d){
      X<-Xnew
      d<-dnew
      D<-Dnew
    }
    else{
      Xnew<-X
    }
  }
  return(list(Xoptim=X,dOptim=d,DOptim=D))
}


#plan d'experience
# PE<-OptimLHS(70,5,10000)
# PE$dOptim

PE1<-OptimLHS1(70,5,10000)
PE1$dOptim
pts1<-PE1$Xoptim

result<-matrix(1,70,1)
 
for(i in 1:70){
  result[i,1]<-compute_wls(pts1[i,])
}
result

library(DiceKriging)
library(DiceOptim)
