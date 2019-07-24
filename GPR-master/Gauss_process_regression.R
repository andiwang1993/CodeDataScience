##Gauss process regression
rm(list=ls()) # to clear the environment

#### loading some packages and functions ####
library("plot3D")
library("MASS")
source("kernFun.R")

x<-seq(0,1,0.01)
y<-x

index<-sort(sample(100,10))
xtest<-x[index]
ytest<-xtest+sin(4*pi*xtest)
f<-x+sin(4*pi*x)

###question 8
param <- c(0.5,0.2)
k<-expKern #choisir la function de kernel
# k(x,xtest,param)

m<-function(k,x,xtest,ytest,param){
  m.result<-k(x,xtest,param)%*%solve(k(xtest,xtest,param)+1e-6*diag(length(xtest)))%*%(ytest)
  # m.result<-k(x,xtest,param)%*%solve(k(xtest,xtest,param))%*%(ytest)
  return(m.result)
}
cfunction<-function(k,x,y,xtest,param){
   c.result<-k(x,y,param)-k(x,xtest,param)%*%solve(k(xtest,xtest,param)+1e-6*diag(length(xtest)))%*%k(xtest,y,param)
  # c.result<-k(x,y,param)-k(x,xtest,param)%*%solve(k(xtest,xtest,param))%*%k(xtest,y,param)
  return(c.result)
}

m1<-m(k,x,xtest,ytest,param)
c1<-cfunction(k,x,x,xtest,param)
xx<-matrix(0,ncol=1,nrow = length(c1[,1]))
xx[1]<-sqrt(c1[1,1])
for(i in 1:length(xx)){
  xx[i]<-sqrt(c1[i,i])
}
xx[is.na(xx)]<-0
plot(x,f,type = "l",xlim = c(0,1),ylim=c(-3,4))
par(new=TRUE)
plot(x,m1,type = "l",xlim = c(0,1),ylim=c(-3,4))
par(new=TRUE)
plot(x,m1-1.96*xx,type = "l",xlim = c(0,1),ylim=c(-3,4),col='red')
par(new=TRUE)
plot(x,m1+1.96*xx,type = "l",xlim = c(0,1),ylim=c(-3,4),col='red')

samples<-t(mvrnorm(100,mu=m1,Sigma=c1))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")

