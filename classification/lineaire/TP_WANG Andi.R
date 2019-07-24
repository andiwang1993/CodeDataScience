library(MASS)
data(iris)
data<-iris
c<-numeric()
for (i in 1:nrow(data)){
  if(data[i,5]=="setosa"){
    c[i]<-1
  }
  if(data[i,5]=="versicolor"){
    c[i]<-2
  }
  if(data[i,5]=="virginica"){
    c[i]<-3
  }
}
pairs(iris,col=iris[,5])
plot(data,col=data[,5])
dat_ana<-data[,-5]
dat.lda<-lda(dat_ana,data[,5])
plot(dat.lda,dimen =1,type="density")
plot(dat.lda,col=as.numeric(data[,5]),abbrev = TRUE)
mlda<-lda(iris$Species~iris$Petal.Length+iris$Petal.Width)
mlda
plot(mlda,col=as.numeric(data[,5]),abbrev = TRUE)
mlda.pred<-predict(mlda)
table(mlda.pred$class,iris$Species)#confusion


library(nnet)
RL<-multinom(iris$Species~.,iris[,1:4])
RL.pred<-predict(RL,iris[,1:4])

table(RL.pred,iris$Species)




n<-nrow(iris)
trainIndex<-sample.int(n,2*n/3)
train<-iris[trainIndex,]
test<-iris[-trainIndex,]

mlog<-multinom(train$Species~.,train)
mlog.pred<-predict(mlog,test[,1:4])

conf<-table(mlog.pred,test$Species)#table de confusion
conf

mlda<-lda(train[,5]~.,train[,1:4])

mlda.pred<-predict(mlda,test[,1:4])
table(mlda.pred$class,test[,5])#confusion
