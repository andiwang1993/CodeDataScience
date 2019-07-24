rm(list = ls())
n<-1000
X1<-runif(n,min=-pi,max=pi)
X2<-runif(n,min=-pi,max=pi)
X3<-runif(n,min=-pi,max=pi)

f<-sin(X1)+7*sin(X2)^2+0.1*(X3)^4*sin(X1)

E<-mean(f)


op<-par(mfrow=c(1,3))
plot(X1,f-E*matrix(1,n,1))
a1<-smooth.spline(X1,f-E*matrix(1,n,1))
lines(a1,col='red')

plot(X2,f-E*matrix(1,n,1))
a2<-smooth.spline(X2,f-E*matrix(1,n,1))
lines(a2,col='red')

plot(X3,f-E*matrix(1,n,1))
a3<-smooth.spline(X3,f-E*matrix(1,n,1))
lines(a3,col='red')

par(op)


#################
library(sensitivity)

f<-function(X,a,b){
  sin(X[,1])+7*sin(X[,2])^2+0.1*(X[,3])^4*sin(X[,1])
  
}
f1<-function(X,a,b){
  X[,1]-2*X[,2]+a*X[,1]*X[,2]+b*X[,1]^1
}

m<-morris(model = f1,factors = 2,r = 500, 
          design = list(type = "oat",     levels = 5, grid.jump = 3),
          binf=-pi,bsup=pi,a=1,b=2)
print(m)
plot(m)
library(rgl)
plot3d(m)

###################
