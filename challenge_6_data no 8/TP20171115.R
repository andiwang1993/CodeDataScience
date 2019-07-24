# library(rpart)
# library(party)
# library(partykit)
# 
# library(xgboost)
# modelXgboost<-xgboost(data=x,)##a completer comment utiliser

##cross validation
library(caret)
library(randomForest)
library(e1071)
library(ranger)
data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
nums <- sapply(data, is.numeric)
data <- data[, nums]



xtrain<-data.frame(data[,1:(ncol(data)-2)])

princomp(xtrain)


datatest <- read.csv2('TestSample.csv')
for(i in 1:ncol(datatest)){
  datatest[is.na(datatest[,i]), i] <- mean(datatest[,i], na.rm = TRUE)
}
nums <- sapply(datatest, is.numeric)
datatest <- datatest[, nums]
ctrlGas<-trainControl(method = "repeatedcv",number = 3,repeats = 1)####
tuneGasGrid <- expand.grid(max_depth = 1:10, nrounds = 150, eta = c(0.01, 0.1, 0.5, 1),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)

# head(tuneGrid)
# gbmGrid<-expand.grid(.interaction.depth=c(1,3),.n.trees=seq(50,300,by=50),.shrinkage=0.1)
yGas<-data$GasCum360

CaretGas <- train(yGas~ ., data = xtrain,
                 method = "rf",
                 tuneLength=20,
                 trControl = ctrlGas)


rangerCtrl <- train(yGas ~ ., 
                data = xtrain,
                method = 'ranger',
                # should be set high at least p/3
                tuneLength = 10, 
                trControl = ctrlGas,
                ## parameters passed onto the ranger function
                # the bigger the better.
                num.trees = 700,
                importance = "permutation")
# gbmfit<-train(xtrain,yGas,method = "gbm",trControl = ctrlGas,tuneGrid = gbmGrid,verbose=F)
# modelctrlGas<-train(yGas~., 
#                     data=xtrain,method="glm")

###bootstrap
