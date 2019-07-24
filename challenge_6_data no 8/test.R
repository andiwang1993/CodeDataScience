train=read.csv2(file = "TrainSample.csv",header = TRUE,sep = ";");
test=read.csv2(file = "TestSample.csv",header = TRUE,sep = ";");

gasmodel<-lm(train$GasCum360~(train$Surf_X+train$Surf_Y+train$Lateral_Length..ft.+train$Depth_TVD_PPLS..ft.))
plot(gasmodel)
testgas <- predict(gasModel, newdata = test, interval = 'pre')

dataMerged <- data.frame(id = train$API,
                           CUM360 = train$OilCum360,
                           GAS360 = train$GasCum360,
                           CUM360_INF = predOil[,2],
                           CUM360_SUP = predOil[,3],
                           GAS360_INF = predGas[,2],
                           GAS360_SUP = predGas[,3])

############TD 18 Oct##############
data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

y <- data$GasCum360


nums <- sapply(x, is.numeric)
data <- data[, nums]
x <- data[, 1:(ncol(data)-2)]

df1 <- data.frame(y = y, x)
df1 <- na.omit(df1)

n <- dim(df1)[1]
trainIndex <- sample(n, floor(n/2))  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)

model <- lm(y ~ ., df1, subset = trainIndex)
plot(model)
summary(model)

fitted <- predict(model, newdata = df1[-trainIndex, ])
observed <- df1$y[-trainIndex]
plot(observed, fitted, ylab = "fitted", xlab = "observed")
abline(a=0, b=1)


#test=read.csv2(file = "TestSample.csv",header = TRUE,sep = ";")
#test=test[,-4:-6]
#for(i in 1:ncol(test)){
#  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
#}

###mean square error


