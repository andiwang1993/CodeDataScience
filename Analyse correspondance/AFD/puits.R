train<-data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

nums <- sapply(data, is.numeric)
data <- data[ , nums] # variables numeriques uniquement

train[,4]<-date_drilling<-as.Date(train[,4],format="%d/%m/%Y")
train[,5]<-date_completion<-as.Date(train[,5],format="%d/%m/%Y")
train[,6]<-date_production<-as.Date(train[,6],format="%d/%m/%Y") 

day1<-as.numeric(date_completion-date_drilling)
day2<-as.numeric(date_production-date_drilling)
day3<-as.numeric(date_production-date_completion)

###

boxplot(train)

#######Etape 3
library(FactoMineR)
hist(train$API)
dat<-data[,-1] ##donnees quantitatives
dat<-scale(dat)
index1=which(train$API>600)
index2=which(train$API<400)

grp1<-dat[index1,]
grp2<-dat[index2,]
grp3<-dat[-c(index1,index2),]

var<-var(dat)
var1<-var(grp1)
var2<-var(grp2)
var3<-var(grp3)

g<-colMeans(dat)
g1<-colMeans(grp1) 
g2<-colMeans(grp2)
g3<-colMeans(grp3)

n<-length(dat[,1])
n1<-length(grp1[,1])
n2<-length(grp2[,1])
n3<-length(grp3[,1])

B<-n1/n*(g1-g)%*%t(g1-g)+n2/n*(g2-g)%*%t(g2-g)+n3/n*(g3-g)%*%t(g3-g)
W<-n1/n*var1+n2/n*var2+n3/n*var3
B+W
var ##pas egale initialement
B+W-var
d<-solve(var)%*%B
d.eig<-eigen(d)
head(d.eig)
valp<-d.eig$values
Re(valp[1:2])
vectp<-d.eig$vectors
a1<-Re(vectp[,1])
a2<-Re(vectp[,2])

I1<-0
for(i in 1:n1){
  I1<-sum((grp1[i,]-g1)^2)+I1
}
I1<-I1/30
I1

I2<-0
for(i in 1:n2){
  I2<-sum((grp2[i,]-g2)^2)+I2
}
I2<-I2/n2
I2

I3<-0
for(i in 1:n3){
  I3<-sum((grp3[i,]-g3)^2)+I3
}
I3<-I3/n3
I3


I<-0
for(i in 1:n){
  I<-sum((dat[i,]-g)^2)+I
}
I<-I/n
I
contrA11<-I1/I*(t(a1)%*%solve(var)%*%g1)^2
contrA12<-I2/I*(t(a1)%*%solve(var)%*%g2)^2
contrA13<-I3/I*(t(a1)%*%solve(var)%*%g3)^2
contrA11
contrA12
contrA13

contrA21<-I1/I*(t(a2)%*%solve(var)%*%g1)^2
contrA22<-I2/I*(t(a2)%*%solve(var)%*%g2)^2
contrA23<-I3/I*(t(a2)%*%solve(var)%*%g3)^2
contrA21
contrA22
contrA23

contrR11<-contrA11/Re(valp[1])
contrR12<-contrA12/Re(valp[1])
contrR13<-contrA13/Re(valp[1])
contrR11
contrR12
contrR13

contrR21<-contrA21/Re(valp[2])
contrR22<-contrA22/Re(valp[2])
contrR23<-contrA23/Re(valp[2])
contrR21
contrR22
contrR23

########3.2
eigen(cor(dat))
a1_ana<-abs(a1)
ind1<-which(a1_ana==max(a1_ana))
a2_ana<-abs(a2)
ind2<-which(a2_ana==max(a2_ana))
ind1
ind2
name<-names(data.frame(dat))
name[ind1]
name[ind2]

#########3.4
library(FactoMineR)
G<-as.character()
index3<-setdiff(1:n,c(index1,index2))
G[index1]<-"A"
G[index2]<-"B"
G[index3]<-"C"
xx<-cbind(G,data[,2:42])
colnames(xx)<-c("grp",colnames(data[,2:42]))

xx<-cbind(G,dat[,1:41]%*%cbind(a1[1:41],a2[1:41]))
colnames(xx)<-c("grp","v1","v2")
model<-FAMD(xx)


test<-read.csv2("TestSample.csv")
for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
}

nums <- sapply(test, is.numeric)
test<- test[ , nums] # variables numeriques uniquement

dat.test<-test[,-1]
G2<-as.character()
inde1<-which(test$API>600)
inde2<-which(test$API<400)
inde3<-setdiff(1:nrow(test),c(inde1,inde2))
G2[inde1]="A"
G2[inde2]="B"
G2[inde3]="C"
tt<-cbind(G2,as.matrix(dat.test)%*%cbind(a1[1:41],a2[1:41]))
colnames(tt)<-c("grp","v1","v2")
tt[,2:3]<-as.numeric(tt[,2:3])

tt<-cbind(G2,dat.test)
colnames(tt)<-c("grp",colnames(dat.test))

pretest<-predict(model,tt)



