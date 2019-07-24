data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
nums <- sapply(data, is.numeric)
data <- data[, nums]
y<- data$GasCum360
x <- data[, 1:(ncol(data)-2)]

df1 <- data.frame(y = y, x = x)
df1 <- na.omit(df1)

n <- dim(df1)[1]
trainIndex <- sample(n, floor(n/2))  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)

Gasmodel <- lm(y~ ., df1, subset = trainIndex)
summary(Gasmodel)

fitted <- predict(Gasmodel, newdata = df1[-trainIndex, ])
observed <- df1$y[-trainIndex]
plot(observed, fitted, ylab = "fitted", xlab = "observed")
abline(a=0, b=1)

#library(tree)
#Gastree<-tree(y~ .,df1,subset = trainIndex)
#predtree<-predict(Gastree, newdata = df1[-trainIndex, ])

rsq<-function(formula, data)
