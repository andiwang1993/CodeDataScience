linKern <- function(x, y, param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma <- param[1]
  kern <- sigma^2*outer(x, y, '*')
  return(kern)
}

cosKern <- function(x, y, param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma <- param[1]
  theta <- param[2]
  dist <- outer(x/theta, y/theta, '-')
  kern <- sigma^2*cos(dist)
  return(kern)
}

expKern <- function(x, y, param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma <- param[1]
  theta <- param[2]
  dist <- outer(x/theta, y/theta, '-')
  kern <- sigma^2*exp(-abs(dist))
  return(kern)
}

seKern<-function(x,y,param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma<-param[1]
  theta<-param[2]
  dist<-outer(x/theta,y/theta,'-')
  kern<-sigma^2*exp(-1/2*dist^2)
  return(kern)
}

mat5_2Kern<-function(x,y, param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma<-param[1]
  theta<-param[2]
  dist<-outer(x/theta,y/theta,'-')
  kern<-sigma^2*(1+sqrt(5)*abs(dist)+5/3*dist^2)*exp(-sqrt(5)*abs(dist))
  return(kern)
}

brownKern<-function(x,y,param){
  # input:
  #  x,y: input vectors
  #  param: parameters sigma
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma<-param[1]
  n1<-length(x)
  n2<-length(y)
  kern<-matrix(0,n1,n2)
  
  for(i in 1:n1){
    for(j in 1:n2){
      kern[i,j]<-sigma^2*min(x[i],y[j])
    }
  }
  return(kern)
}

sincKern<-function(x,y,param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma<-param[1]
  theta<-param[2]
  dist<-outer(x/theta,y/theta,'-')
  
  kern<-sigma^2*1/dist*sin(dist)
  kern[is.na(kern)]<-sigma^2
  return(kern)
}

Kern5_1<-function(x,y,param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma<-param[1]
  theta<-param[2]
  n1<-length(x)
  n2<-length(y)
  kern<-matrix(0,n2,n1)
  for(i in 1:n1){
    for(j in 1:n2){
      dist<-outer((y[j]-1)^2/theta,(x[i]-1)^2/theta,'-')
      kern[j,i]<-sigma^2*cos(sqrt(abs(dist)))
    }
  }
  return(kern)
}
Kern5_2 <- function(x, y, param){
  # input:
  #  x,y: input vectors
  #  param: parameters (sigma,theta)
  # output:
  #  kern: covariance matrix cov(x,y)
  sigma <- param[1]
  theta <- param[2]
  dist <- outer(y*2,2*x, '-')
  kern <- sigma^2*cos(dist)
  return(kern)
}

kernBon<-function(y,param,t){
  phi<-param[1]
  n<-length(y)
  kern<-matrix(0,n,n)
  mu<-t[1]-1
  for(i in 1:n){
    for(j in 1:n){
      kern[i,j]<-phi^abs(i-j)*(phi-phi^(mu+min(i,j)))
    }
  }
  return(kern)
}