data<-read.csv('data_student.csv')
##############question 1#########
pairs(data,col=data[,15])

dat_ana<-data[,-15]

library(MASS)
dat.lda<-lda(dat_ana,data[,15])
plot(dat.lda,dimen =1,type="density")
plot(dat.lda,col=as.numeric(data[,15]))

ldapred<-predict(dat.lda,newdata=dat_ana)
table(ldapred$class,data[,15])#confusion


library(nnet)
RL<-multinom(data[,15]~.,data[,1:14])

RL.pred<-predict(RL,data[,1:14])


table(RL.pred,data[,15])

#kmeans
cl7<-kmeans(dat_ana,7)
cl7$cluster
table(cl7$cluster,data[,15])
cl2<-kmeans(dat_ana,2)
cl2$cluster
table(cl2$cluster,data[,15])

#################question 2
#kmean
dat_ana2<-scale(dat_ana)

mkmeans<-kmeans(dat_ana2,7)
table(mkmeans$cluster,data[,15])

mkmeans3<-kmeans(dat_ana2,3)
table(mkmeans3$cluster,data[,15])

mkmeans9<-kmeans(dat_ana2,9)
table(mkmeans9$cluster,data[,15])

mkmeans2<-kmeans(dat_ana2,2)
table(mkmeans2$cluster,data[,15])



#hierarchie
d<-dist(dat_ana2,method = "maximum") #method manhattan, maximum, euclidean
hc1 <- hclust(d,"ward")
plot(hc1,labels=FALSE)
cu<-cutree(hc1,3) #test 2 3 7
table(cu,data[,15])

hc2 <- hclust(d,"ave")
plot(hc2)

hc3 <- hclust(d,"median")
plot(hc3)

hc4 <- hclust(d,"centroid")
plot(hc4)
#k-medoids
library(clue)
library(cluster)

m2 <- pam(dat_ana2,2)
table(m2$clustering,data[,15])
m3 <- pam(dat_ana2,3)
table(m3$clustering,data[,15])
m7 <- pam(dat_ana2,7)
table(m7$clustering,data[,15])

#knn
n <- nrow(data)

I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on pr¨¦pare les donn¨¦es : on construit le classifieur K-NN
# pour les valeurs num¨¦rique et on extrait explicitement la classe 
# ¨¤ predire

cl <- data[I,15]

dlrn <- data[I,1:14]
dtest <- data[J,1:14]
mknn1 <- knn(dlrn, dtest,cl, k=1)
mknn1

table(mknn1, data[J,15])


mknn3 <- knn(dlrn, dtest,cl, k=3)
mknn3

table(mknn3, data[J,15])

mknn7 <- knn(dlrn, dtest,cl, k=7)
mknn7

table(mknn7, data[J,15])

mknn11 <- knn(dlrn, dtest,cl, k=11)
mknn11

table(mknn11, data[J,15])

#cross validation
train <- data[,1:14]
cl <- data[,15]
model <- knn.cv(train,cl,k=5)
model
table(model,data[,15])


################question 3
n<-nrow(data)
trainIndex<-sample.int(n,2*n/3)
train<-data[trainIndex,]
test<-data[-trainIndex,]
#lda
mlda<-lda(train[,1:14],train[,15])
predlda<-predict(mlda,test[,1:14])
table(predlda$class,test[,15])
#logistic
RL<-multinom(train[,15]~.,train[,1:14])
RL.pred<-predict(RL,test[,1:14])


table(RL.pred,test[,15])


##knn cv already done in the former question
#kmeans
mkme<-kmeans(train[,1:14],2)

pre<-kmeans(test[,1:14],2)
table(pre$cluster,test[,15])

new=(cbind(data[,1],data[,11]))

#lda
mlda<-lda(new,data[,15])
predlda<-predict(mlda,new)
table(predlda$class,data[,15])
#logistic
RL<-multinom(data[,15]~.,data.frame(new))
RL.pred<-predict(RL,data.frame(new))


table(RL.pred,data[,15])

m2 <- pam(scale(new),2)
table(m2$clustering,data[,15])

pre<-kmeans(new,2)
table(pre$cluster,data[,15])

model <- knn.cv(new,cl,k=5)
model
table(model,data[,15])
