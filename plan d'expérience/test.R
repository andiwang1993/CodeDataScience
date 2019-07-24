###computer lab#########
library(sensitivity)
n<-1000
X1<-runif(n,min=0,max=1)
X2<-runif(n,min=0,max=1)
X3<-runif(n,min=0,max=1)

fvalue<-X1*X2*X3

E<-mean(fvalue)


op<-par(mfrow=c(1,3))
plot(X1,fvalue-E*matrix(1,n,1))
a1<-smooth.spline(X1,fvalue-E*matrix(1,n,1))
lines(a1,col='red',cex=2)

plot(X2,fvalue-E*matrix(1,n,1))
a2<-smooth.spline(X2,fvalue-E*matrix(1,n,1))
lines(a2,col='red',cex=2)

plot(X3,fvalue-E*matrix(1,n,1))
a3<-smooth.spline(X3,fvalue-E*matrix(1,n,1))
lines(a3,col='red',cex=2)

par(op)

f2<-function(X){
  X[,1]*X[,2]*X[,3]
  
}
m1<-morris(model = f2,factors = 3,r = 500, 
          design = list(type = "oat",     levels = 5, grid.jump = 3),
          binf=0,bsup=1)
print(m1)
plot(m1,xlim=c(0.240,0.265))
So1<-fast99(model = f2, factors = 3, n = 1000,
            q = "qunif", q.arg = list(min = 0, max = 1))
print(So1)
plot(So1)

library(DiceKriging)
X<-cbind(X1,X2,X3)
y<-X1*X2*X3
model<-km(~1,design=X,y)
plot(model)


# expand.grid()
X5<-X4<-seq(0,1,length.out = 100)
d<-expand.grid(X4,X5)
#############computer lab###############
Ishigami<-function(X,A,B){
  sin(X[,1])+A*sin(X[,2])^2+B*X[,3]^4*sin(X[,1])
}
m<-morris(model = Ishigami,factors = 3,r = 500, 
          design = list(type = "oat",     levels = 5, grid.jump = 3),
          binf=-pi,bsup=pi,A=7,B=0.1)
print(m)
plot(m)

So<-fast99(model = Ishigami, factors = 3, n = 1000,
                  q = "qunif", q.arg = list(min = -pi, max = pi),A=7,B=0.1)
print(So)
plot(So)


####Kriging######
library(DiceDesign)
dimension <- 3
n <- 30
rm(X)
# X <- lhsDesign(n,dimension)$design

##construire la matrice grande pour X
x1<-x2<-x3<-seq(0,1,length.out=100)
X<-expand.grid(x1,x2,x3)
X<-as.matrix(X)
Xopt <- maximinESE_LHS(X,T0=0.005*phiP(X),inner_it=100,J=50,it=2)

plot(Xopt$design)
plot(Xopt$critValues,type="l")
