## set of test functions

# Ackley function. Global minimum at glob_xstar
ackley <- function(xx, a=20, b=0.2, c=2*pi){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  xx <- xx - glob_xstar
  aa <- 6.4*xx
  d <- length(aa)
  sum <- -a*exp(-b*sqrt((1/d)*sum(aa*aa))) - exp((1/d)*sum(cos(c*aa)))+ a + exp(1)
  y <- sum + noise
  return(y)
}

## Rastrigin function. Global minimum at glob_xstar
rastrigin <- function(xx){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  scaling_fact <- 1.024
  xx <- xx - glob_xstar
  aa <- scaling_fact*xx
  d <- length(aa)
  sum <- sum(aa*aa - 10*cos(2*pi*aa))
  y <- sum + 10*d
  return(y + noise)
}

## Schwefel function. Global Minimum at glob_xstar

schwefel <- function(xx){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  xx <- xx - glob_xstar + 1
  aa <- 100*xx
  d <- length(aa)
  sum <- 418.9829*d - sum(aa*sin(sqrt(abs(aa))))
  y <- sum
  return(y + noise)
}

## Sphere function. Global Minimum at glob_xstar

sphere <- function(xx){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  xx <- xx - glob_xstar
  #aa <- 1.024*xx
  aa <- xx
  d <- length(aa)
  sum <- sum(aa*aa)
  y <- sum
  return(y + noise)
}

##### michalewicz function #########
#glob min in 2D: -1.83, 5D: -4.71 , 10D: -9.64
michalewicz <- function(xx, m=10){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  aa <- 0.1*pi*(xx + 5)
#   aa <- xx
  d <- length(aa)
  i <- 1:d
  sum <- sum(sin(aa)*sin((i*aa*aa)/pi)^(2*m))
  y <- -sum
  return(y + 2 + noise)
}

##### quadratic function #########"
quadratic <- function(xx){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  cond.no <- 1.e3
  aa <- 1.024*xx
  d <- length(aa)
  #  xstar <- 2.5 # default value, change it if needed
  xstar <- glob_xstar
  lambdas <- diag(seq(1, cond.no, ,d))
  # matrix with arbitrary orientation. The seed number decides the orientation.
  if (!exists("glob_umat")) {
    set.seed(1) # change this seed to change the orientation of the quadratic function
    glob_umat <<- qr.Q(qr(matrix(runif(d*d),nrow=d,ncol=d)))
    glob_umat <<- glob_umat[,sample(seq(1,d))]
    #  glob_umat <<- diag(1, d) # to generate a quadratic function whose principal axes are aligned with coordinates
  }
  H <- glob_umat%*%lambdas%*%t(glob_umat)  
  y <- 0.5*t(aa - xstar)%*%H%*%(aa - xstar)
  return(y + noise)
}

#### Tunnel function #######"
tunnel <- function(xx, b=0.5){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  d <- length(xx)
  xx <- xx - glob_xstar
  in_tunnel <- TRUE
  if (d > 1) {
    for(i in 2:d){
      if ((xx[i] < -b) | (xx[i] > b)){
        in_tunnel <- FALSE
      }
    }
    if (in_tunnel) {
      y <- -exp(-norm(as.matrix(xx), type="F")/10)
    } else{
      y <- t(xx)%*%xx
    }
  } else{
    y <- xx^2
  }
  return(y+noise)
}


### 

####exe2 function#######
###pp<-matrix(round(runif(n*zdim,min=-1000,max=1000)),n,zdim)


exe2<-function(xx,n=5,zdim=5){
  if (glob_noisy) {
    noise <-  rnorm(1, 0, glob_tau) 
  } else noise <- 0
  ##pp<-matrix(round(runif(n*zdim,min=-1000,max=1000)),n,zdim)
  pp<-matrix(0,n,zdim)
  ####matrice pp pour tests
  pp[1,]<-c(-6,1,4,8,-7)
  pp[2,]<-c(-3,-4,2,1,0)
  pp[3,]<-c(-8,-4,1,-8,-9)
  pp[4,]<-c(-8,9,-4,-6,4)
  pp[5,]<-c(1,2,-2,3,-2)
  zz<-matrix(0,n,1)
  
  for(i in 1:n){
    zz<-sqrt(sum(t(xx-pp[i,])*(xx-pp[i,])))
  }
  y<- -min(zz)
  return(y+noise)
}