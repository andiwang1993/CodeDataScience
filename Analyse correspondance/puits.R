train<-read.csv2("TrainSample.csv")
date_drilling<-as.Date(train[,4],format="%d/%m/%Y")
date_completion<-as.Date(train[,5],format="%d/%m/%Y")
date_production<-as.Date(train[,6],format="%d/%m/%Y") 

day1<-as.numeric(date_completion-date_drilling)
day2<-as.numeric(date_production-date_drilling)
day3<-as.numeric(date_production-date_completion)

###


hist(train_chiffre)
boxplot(train_chiffre)
