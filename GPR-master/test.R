rm(list=ls()) # to clear the environment

#### loading some packages and functions ####
library("plot3D")
library("MASS")
source("kernFun.R")
x<-mvrnorm(100,rep(0,1),Sigma=1,)
y<-mvrnorm(100,Sigma=1)
