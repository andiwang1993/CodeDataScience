data1<-read.csv2("chienloup.csv")
dat<-as.matrix(data1[,-1])
n<-42

dat_loup<-dat[31:42,]##group 1

n1<-as.numeric(dim(dat_loup)[1])

dat_chien<-dat[1:30,]##group 2
n2<-as.numeric(dim(dat_chien)[1])

#############Question 1)###################
##V=W+B
#Groupe 1 loup
var.total<-cov(dat)
var1<-cov(dat_loup)
var2<-cov(dat_chien)
g<-colMeans(dat)
g1<-colMeans(dat_loup) 
g2<-colMeans(dat_chien)
B<-n1/n*(g1-g)%*%t(g1-g)+n2/n*(g2-g)%*%t(g2-g)
W<-n1/n*var1+n2/n*var2
B+W
var.total ##pas egale initialement
B+W-var.total
d<-solve(var.total)%*%B

################dimension et valeurs propres et vecteurs propres de V^(-1) et de B
dim(solve(var.total))
eigen(solve(var.total))
dim(B)
eigen(B)

###################diagonaliser V^(-1)*B
d.valp<-eigen(d)$values #pas reel pas diagonalisable
d.valp
d.vecp<-eigen(d)$vectors
d.vecp
#################
library(ade4)
library(FactoMineR)
K<-rbind(matrix(2,n2,1),matrix(1,n1,1))
data2<-scale(dat)

model1<-dudi.mix(data2)
model1$eig
model1$co

dat2<-data1
dat2[,2:7]<-scale(dat2[,2:7])
model2<-FAMD(data.frame(dat2))

plot(model2)
model2$eig
model2$var

summary(model2)
#####Question 3)
dat2<-scale(dat)
dat2.var<-cov(dat2)
dat2.var1<-cov(dat2[1:30,])
dat2.var2<-cov(dat2[31:42,])
dat2.g<-colMeans(dat2)
dat2.g1<-colMeans(dat2[1:30,]) 
dat2.g2<-colMeans(dat2[31:42,])
dat2.B<-30/n*(dat2.g1-dat2.g)%*%t(dat2.g1-dat2.g)+12/n*(dat2.g2-dat2.g)%*%t(dat2.g2-dat2.g)
dat2.W<-30/n*dat2.var1+12/n*dat2.var2
dat2.B+dat2.W
dat2.var ##pas egale initialement
dat2.B+dat2.W-dat2.var
dat2.d<-solve(dat2.var)%*%dat2.B
d2.eig<-eigen(dat2.d)
d2.eig
dat2.valp<-d2.eig$values

dat2.vectp<-d2.eig$vectors
# dat2.eig<-eigen(dat2.var)
# dat2.eig
# var<-cov(dat2)
# valp3<-eigen(var)$values
# valp3
# vectp3<-eigen(var)$vectors
# vectp3

as<-dat2.vectp[,6]
z<-scale(dat)%*%as
m.z1<-mean(z[1:30])
m.z2<-mean(z[31:42])
m.z<-mean(z)
SCT<-0
for(i in 1:42){
  SCT<-SCT+(z[i]-m.z)^2
}
SCE<-30*(m.z1-m.z)^2+12*(m.z2-m.z)^2
ita<-SCE/SCT
ita

eigen(solve(dat2.var)%*%dat2.B)
eigen(solve(dat2.W)%*%dat2.B)


########
dat2<-scale((dat))
dat2.var<-cov(dat2)
dat2.eig<-eigen(dat2.var)
dat2.eig
dat2.cor<-cor(dat2)
dat2.cor
coor<-dat2%*%dat2.eig$vectors[,1:5]

# ncor<-model2$ind$coord
ncor<-coor
ncor.var<-cov(ncor)
ncor1<-ncor[1:30,]
ncor2<-ncor[31:42,]
v1<-cov(ncor1)
v2<-cov(ncor2)
WW<-30/42*v1+12/42*v2


gg<-colMeans(ncor)
gg1<-colMeans(ncor1)
gg2<-colMeans(ncor2)

BB<-30/42*(gg1-gg)%*%t(gg1-gg)+12/42*(gg2-gg)%*%t(gg2-gg)

WW+BB
ncor.var
WW+BB-ncor.var #C est mieux que des donnees dans question 1)

###
eig1<-eigen(solve(WW)%*%BB)
eig1
eig2<-eigen(solve(ncor.var)%*%BB)
eig2 ###pas tous reel mais presque vers 0 sauf le premier valeur propre
##axes discriminant est etabli par la premiere vecteur
ncor%*%eig2$vectors[,1]
######
#########
ncor.eig<-eigen(ncor.var)
ncor.eig #vecteur propres vers I5

ncor.cor<-cor(ncor) ###vers I5 presque independant
ncor.cor
####
###########question 4)
###qualite de la projection
dat2.valp/sum(dat2.valp)

dat2.I2<-0
for(i in 31:42){
  dat2.I2<-sum((dat2[i,]-dat2.g2)^2)+dat2.I2
}
dat2.I2<-dat2.I2/12
dat2.I2

dat2.I1<-0
for(i in 1:30){
  dat2.I1<-sum((dat2[i,]-dat2.g1)^2)+dat2.I1
}
dat2.I1<-dat2.I1/30
dat2.I1
dat2.I<-0
for(i in 1:42){
  dat2.I<-sum((dat2[i,]-dat2.g)^2)+dat2.I
}
dat2.I<-dat2.I/42
dat2.I

contrA1<-dat2.I1/dat2.I*(t(dat2.vectp[,1])%*%solve(dat2.var)%*%dat2.g1)^2
contrA2<-dat2.I2/dat2.I*(t(dat2.vectp[,1])%*%solve(dat2.var)%*%dat2.g2)^2
contrA1
contrA2
  
contrR1<-contrA1/dat2.valp[1]
contrR2<-contrA2/dat2.valp[1]
contrR1
contrR2




##question 5)


chien_5<-scale(dat_chien)
loup_5<-scale(dat_loup)
dat5<-rbind(chien_5,loup_5)
dat5.var<-cov(dat5)
dat5.var1<-cov(chien_5)
dat5.var2<-cov(loup_5)
dat5.g<-colMeans(dat5)
dat5.g1<-colMeans(chien_5) 
dat5.g2<-colMeans(loup_5)
dat5.B<-30/n*(dat5.g1-dat5.g)%*%t(dat5.g1-dat5.g)+12/n*(dat5.g2-dat5.g)%*%t(dat5.g2-dat5.g)
dat5.W<-30/n*dat5.var1+12/n*dat5.var2
dat5.B+dat5.W
dat5.var 
dat5.B+dat5.W-dat5.var
dat5.d<-solve(dat5.var)%*%dat5.B
dat5.d.eig<-eigen(dat5.d)
dat5.d.eig
eigen(solve(dat5.var)%*%dat5.W)
vectp5<-dat5.d.eig$vectors
dat5.as<-vectp5[,1]


model5<-dudi.mix(dat5)
