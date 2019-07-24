# 4.1
#echan<-c(52,10,40,104,50,27,146,31,46,94,38,23,197,99,16,141)
###
echan<-rnorm(10000,mean = 10,sd=2)


B <- 2000
resample <- numeric()
sd_res<-numeric()
IC<-matrix(0,B,2)
alpha<-0.95
##souris
for (i in 1:B)
{
  n<-10
  data<-sample(echan, size=n, replace=T)
  resample[i] <- mean(data)##resampling and get the average
  sd_res[i]<-sd(data)
  IC[i,]<-c(resample[i]-qnorm(1/2+alpha/2)*sd_res[i]/sqrt(n),resample[i]+qnorm(1/2+alpha/2)*sd_res[i]/sqrt(n))
  
}



moy<-mean(resample)

interConf<-1/B*(c(sum(IC[,1]),sum(IC[,2])));
##loi normal

for (i in 1:B)
{
  n<-1000
  data<-sample(echan, size=n, replace=T)
  resample[i] <- mean(data)##resampling and get the average
  sd_res[i]<-sd(data)
  IC[i,]<-c(resample[i]-qnorm(1/2+alpha/2)*sd_res[i],resample[i]+qnorm(1/2+alpha/2)*sd_res[i])
  
}



moy<-mean(resample)

interConf<-1/B*(c(sum(IC[,1]),sum(IC[,2])));

#4.2


# define Bootstrap replicates number
B <- 2000
resample <- numeric()
IC<-matrix(0,B,2)
for (i in 1:B)
{
  data<-sample(echan, size=1000, replace=T)
  resample[i] <- mean(data)##resampling and get the average
  IC[i,1:2]<-quantile(data,c(0.025,0.975))
  
}
interConf<-1/B*(c(sum(IC[,1]),sum(IC[,2])));
moy<-mean(resample)

####utilisant des donn¨¦es de criminalit¨¦s####
crime<-read.csv2("crime_data_pca.csv")
dat_crime<-(data.frame(crime[,2:14]))
cormat<-cor(dat_crime)
quantile(cormat,c(0.10,0.9))
B <- 2000
resample <- numeric()
IC<-matrix(0,B,2)
for (i in 1:B)
{
  data<-sample(cormat, size=1000, replace=T)
  resample[i] <- mean(data)##resampling and get the average
  IC[i,1:2]<-quantile(data,c(0.10,0.90))
  
}
interConf<-1/B*(c(sum(IC[,1]),sum(IC[,2])));
moy<-mean(resample)
###boucle
x0<-seq(from=10, to=1000,by=10)


interConf<-matrix(0,100,2)
for (j in 1:100){
B<-x0[j]
resample <- numeric()
IC<-matrix(0,B,2)

for (i in 1:B)
{
  data<-sample(cormat, size=20, replace=T)
  resample[i] <- mean(data)##resampling and get the average
  IC[i,1:2]<-quantile(data,c(0.10,0.90))
  
}
interConf[j,]<-1/B*(c(sum(IC[,1]),sum(IC[,2])));
moy<-mean(resample)
}
plot(x0,interConf[,1],ylim=c(-1,1))
par(new=TRUE)
plot(x0,interConf[,2],ylim=c(-1,1))
#####################


#####4.3####
varmat<-var(dat_crime)
valeurp<-eigen(varmat)$values
B<-10000
library("FactoMineR")
library("factoextra")

#kc<-matrix(0,13,1)
test1_inf<-matrix(0,13,1)
test1_sup<-matrix(0,13,1)
for (k in 1:13){
dat1<-matrix(0,B,k)
dat2<-matrix(0,B,k+1)
qua1<-matrix(0,B,2)
qua2<-matrix(0,B,2)
for (i in 1:B)
{
  
  dat1[i,]<-sample(valeurp, size=k, replace=T)
  qua1[i,]<-quantile(dat1[i,],c(0.05,0.95))
  #dat2[i,]<-sample(valeurp, size=k+1, replace=T)
  #qua2[i,]<-quantile(dat2[i,],c(0.05,0.95))
}


test1_inf[k]<-mean(qua1[,1])
test1_sup[k]<-mean(qua1[,2])
#test2_inf[k]<-mean(qua2[,2])
#test2_inf[k]<-mean(qua2[,2])
}
