rm(list=ls()) # to clear the environment

#### loading some packages and functions ####
library("plot3D")
library("MASS")
source("kernFun.R")

#### Example with the Exp. kernel  ####
x <- seq(0, 1, 0.01) # regular grid
y<-x
param <- c(1,0.2) # covariance parameters
k1 <- seKern(x, x, param) # computing the covariance matrix using an exp. kernel
image2D(k1, theta = 90, xlab = "x", ylab = "y") # plotting the covariance matrix
samples<-t(mvrnorm(100,mu=rep(0,nrow(k1)),Sigma=k1))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")
# Q: what can you observe from the covariance matrix?
# ?mvrnorm # using the help from RStudio

## to complete  ##
## simulating some samples using the "mvrnorm" function


param <- c(1,0.2)
x<-seq(0.5,1.5,0.01)
y<-seq(0,1,0.01)
k2<-Kern5_1(x,y,param)
k2
image2D(k2, theta = 90,y=seq(0.5,1.5,length.out=ncol(k2)),xlab = "x", ylab = "y")

samples<-t(mvrnorm(100,mu=rep(0,nrow(k2)),Sigma=k2))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")
# samples <- mvrnorm(...)
# ?matplot # a function to plot the samples. The samples are indexed by columns
# Q: what can you observe from the samples?


##question 4)
x1<-seq(0,10,0.001)
y1<-x1
index<-seq(1,101,1)
x<-x1[index]
alpha<-0
y<-y1[index]+3000*matrix(0.001,length(x),1)
k3<-sincKern(x,y,param)
image2D(k3,theta = 90, xlab = "x", ylab = "y")



##question 5)
param <- c(1,0.2)
x<-seq(0,1,0.01)*2
y<-seq(0,1,0.01)*2
k2<-Kern5_1(x,y,param)
k2
image2D(k2, theta = 90,y=seq(0,2,length.out=ncol(k2)),x=seq(0,2,length.out=nrow(k2)),xlab = "x", ylab = "y")

samples<-t(mvrnorm(100,mu=rep(0,nrow(k2)),Sigma=k2))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")
###
param <- c(1,0.2)
x5<-seq(0,1,0.01)*4*pi
y5<-seq(0,1,0.01)*4*pi
k3<-Kern5_2(x5,y5,param)
k3
image2D(k3, theta = 90,y=seq(0,4*pi,length.out=ncol(k2)),x=seq(0,4*pi,length.out=nrow(k2)),xlab = "x", ylab = "y")

samples<-t(mvrnorm(100,mu=rep(0,nrow(k3)),Sigma=k3))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")


##bonus
t<-seq(1,200,1)
y<-matrix(0,length(t),1)
y[1]<-rnorm(1)
epsi<-rnorm(length(t))
param<-0.6
for(i in 2:100){
  phi<-param[1]
  y[i]<-phi*y[i-1]+epsi[i]
}
y<-matrix(0,length(t),1)
x<-t+cos(2*pi*t)+y
kb<-kernBon(y,param,t)
image2D(kb, theta = 90, xlab = "x", ylab = "y")

samples<-t(mvrnorm(100,mu=rep(0,nrow(kb)),Sigma=kb))
matplot(x,samples[,seq(10)],type = "l",ylab="samples")
