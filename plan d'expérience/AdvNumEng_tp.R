library(DiceDesign)
library(sensitivity)
source("mainScript_DatascienceClass.R")
#' MOGI(G,nu,xs,ys,zs,a,p,xi,yi,zi) compute surface displacements and tilts created by
#' a point source located beneath a topography. To account for topography, a first order
#' solution in which the actual source to ground surface point is taken into account.
#' @author V. Cayol, LMV, sept 2017 (translated into R by R. Le Riche)
#' @example [uxi,uyi,uzi] = mogi_3D(G,nu,xs,ys,zs,a,p,xi,yi,zi)
#' @param G = shear modulus in MPa, G = E/2(1+nu)
#' @param nu = Poisson's ratio
#' @param xs, ys, zs = source position (z axis is positive upward),
#' @param a = source radius,
#' @param p = source overpressure in MPa,
#' @param xi, yi, zi = location of ground surface points
#' @return displacement U(x,y,z) following Mogi's model
mogi_3D <- function(G = 2000,nu = 0.25,xs,ys,zs,a,p,xi,yi,zi){
  DV = pi*a^3*p/G
  C = (1-nu)*DV/pi
  r = sqrt((xi-xs)^2+(yi-ys)^2)
  f = r^2+(zi-zs)^2
  uzi = C*(zi-zs)/(f^(3/2))
  ur = C*r/(f^(3/2))
  theta = atan2(yi-ys,xi-xs)
  uxi = ur*cos(theta)
  uyi = ur*sin(theta)
  U = list(x=uxi,y=uyi,z=uzi)
  return(U)
}

## Get measured data from InSAR
data <- R.matlab::readMat('data_nonoise.mat')
Glb_xi <<- as.matrix(data$locdata[,1])
Glb_yi <<- as.matrix(data$locdata[,2])
Glb_zi <<- as.matrix(data$locdata[,3])
Glb_ulos <<- as.matrix(data$locdata[,4])
# calculate data Covariance matrix, store it in a Global variable
# covariance from exponential kernel, var = 5e-4m2, cor_length = 850 m
# and invert it
Xdata <- cbind(Glb_xi,Glb_yi) # z's are not accounted for in kernel
source("kernels.R")
Glb_CXinv <- solve(kExp(Xdata,Xdata,c(5e-4,850,850))) # calculated once for all, used in wls_ulos
nlos = c(-0.664,-0.168,0.728) # vector of direction of line of sight (satellite)

#' Weighted Least Squares distance function for ulos vectors.
#' The covariance matrix is passed through global variable.
#' @param xyzap array containing xs,ys,zs,a and p
#' @return error (weighted least square) between measure and model
wls_ulos <- function(xyzap){
  G = 2000 # Shear modulus in MPa
  nu = 0.25 # Poisson's ratio
  # Compute surface displacements follwoing Mogi's model
  # nlos<-c(-0.664,-0.168,0.728)
  U <- mogi_3D(G,nu,xyzap[1],xyzap[2],xyzap[3],xyzap[4],xyzap[5],Glb_xi,Glb_yi,Glb_zi)
  # project along LOS
  ulos <- nlos[1]*U$x + nlos[2]*U$y + nlos[3]*U$z
  # calculate weighted least squares error between measure and model
  wls <- t((ulos-Glb_ulos))%*%Glb_CXinv%*%(ulos-Glb_ulos)
  return(wls)
}



x4<-runif(100,0,1)
x5<-runif(100,0,1)

xrandom<-cbind(x1,x2,x3,x4,x5)



Xopt <- maximinESE_LHS(xrandom,T0=0.005*phiP(xrandom),inner_it=100,J=30,it=2)
plot(Xopt$design)
plot(Xopt$critValues,type="l")

y<-compute_wls(xrandom)
library(DiceKriging)
model<-km(~1,design=xrandom,y)
print(model)
plot(model)

kriging.mean<-function(Xnew,m){
  predict(model,Xnew,"UK",se.compute=FALSE,checkNames=FALSE)$mean
}

S<-fast99(model = kriging.mean, factors = 5, n = 1000,
          q = "qunif", q.arg = list(min = 0, max =1),m=model)



#############################use EGO

library(DiceOptim)
set.seed(123)

# a 9-points factorial design, and the corresponding response
d <- 5
n <- 100
x1<-runif(100,0,1)
x2<-runif(100,0,1)
x3<-runif(100,0,1)
design.fact <- cbind(x1,x2,x3,expand.grid(seq(0,1,length=10), seq(0,1,length=10)))

design.fact <- data.frame(design.fact)
names(design.fact)<-c("x", "y","z","a","p")
response.branin <- data.frame(apply(design.fact, 1, compute_wls))
names(response.branin) <- "y"

# model identification
fitted.model1 <- km(~1, design=design.fact, response=response.branin,
                    covtype="matern5_2", control=list(pop.size=50,trace=FALSE), parinit=c(0.5, 0.5))

### EGO##################
library(rgenoud)
nsteps <- 25
lower <- rep(0,d)
upper <- rep(1,d)
oEGO <- EGO.nsteps(model=fitted.model1, fun=compute_wls, nsteps=nsteps,
                   lower=lower, upper=upper, control=list(pop.size=20, BFGSburnin=2))
print(oEGO$par)
print(oEGO$value)

library(plotly)
dat<-data.frame(oEGO$par,oEGO$value)
# plot_ly(dat,type="parcoords",dimensions=list(
#   list(label="x",range=c(-1,1),values=-dat[,1]),
#   list(label="y",range=c(-1,1),values=-dat[,2]),
#   list(label="z",range=c(-1,1),values=-dat[,3]),
#   list(label="a",range=c(-1,1),values=-dat[,4]),
#   list(label="p",range=c(-1,1),values=-dat[,5]),
#   list(label="norm",range=c(-2,2),values=-dat[,6])
# )
#   )

library(MASS)
parcoord(dat)

library(pairsD3)
pairsD3(dat[,1:5])
#######################

library(KrigInv)
set.seed(123)
mbichon<-max_infill_criterion(lower=lower, upper=upper,method = "bichon",T=80,model=fitted.model1)

#####################
max_sur()