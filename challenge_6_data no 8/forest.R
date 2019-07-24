## generate the data

data <- read.csv2('TrainSample.csv')
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
nums <- sapply(data, is.numeric)
data <- data[, nums]
y<- data$GasCum360
x <- data[, 1:(ncol(data)-2)]

x <- data.frame( x)
x <- na.omit(x)

n <- dim(x)[1]
trainIndex <- sample(n, floor(n/2))  # sampling without replacement 
testIndex <- setdiff(1:n, trainIndex)

xnew <- x[-trainIndex,]

#lines(xnew, fun(xnew), lty="dotted")


## estimation d'un arbre de regression
library(tree)
mtree <- tree(y~., x,subset = trainIndex)
p <- predict(mtree, newdata = x[-trainIndex, ])
par(mfrow = c(2,1))
plot(mtree)
text(mtree)
####plot(x, y)
#lines(xnew, p, col = "blue")


## effect of boostrapping on the tree
treeBoot <- function(x, y, control = tree.control(nrow(x), ...), plot = TRUE, ...){
  N <- nrow(x)
  indBoot <- sample(1:N, N, replace = TRUE)
  xBoot <- x[indBoot,]
  yBoot <- y[indBoot]
  mtreeBoot <- tree(y~., x, subset = indBoot,
                    control = control)
  pBoot <- predict(mtreeBoot, data.frame(xnew))
  if (plot) {
    points(x, y, pch = 19, cex = 0.5)
    col <- sample(2:8, 1)
    points(xBoot, yBoot, col = col, pch = 19, cex = 0.5)
    lines(xnew, pBoot, col = col, ...)
  }
  invisible(pBoot)
}

par(mfrow = c(2,1))
###plot(x, y, cex = 0.5)
###treeBoot(x, y)
treeBoot(x, y, control = tree.control(nobs = nrow(x), mincut = 1, minsize = 2))

# bagging trees
B <- 500
pBoot <- matrix(NA, nrow(xnew), B)

##plot(x, y)
for (b in 1:B){
  pBoot[, b] <- treeBoot(x, y, plot = TRUE, 
                         control = tree.control(nobs = nrow(x), 
                                                mincut = 1, minsize = 2),
                         lty = "dotted", lwd = 0.2)
}
pBagging <- rowMeans(pBoot)
lines(xnew, pBagging, lwd = 3, col = "blue")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")


library(randomForest)  # of course here, feature sampling is useless
library(grf)

## calcul d'intervalle de confiance par bootstrap
rfPredBoot <- function(x, y, xnew, package = c("randomForest", "grf"), 
                       nboot = 100){
  
  n <- nrow(x)
  nNew <- nrow(xnew)
  bootPred <- bootConf <- matrix(NA, nrow = nNew, ncol = nboot)
  
  for (i in 1:nboot){
    index <- sample(n, replace = TRUE)
    if (package == "randomForest"){
      mboot <- randomForest(y~x, data = data.frame(x = x[index], y = y[index]))
      bootConf[, i] <- predict(mboot, newdata = data.frame(x = xnew))
      bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sqrt(mboot$mse))
    } else if (package == "grf"){
      mboot <- regression_forest(X = matrix(x[index], ncol = 1), Y = y[index])
      pboot <- predict(mboot, matrix(xnew, ncol = 1))$predictions
      bootConf[, i] <- pboot[, 1]
      sdBoot <- sd(predict(mboot)$predictions-y[index])
      bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sdBoot)
    }
  }
  pboot <- matrix(NA, nrow = nNew, ncol = 5)
  pboot[, 1] <- rowMeans(bootConf)
  sdConfBoot <- apply(bootConf, 1, sd)
  sdPredBoot <- apply(bootPred, 1, sd)
  pboot[, 2] <- pboot[, 1] - 2* sdConfBoot # apply(Mboot, 1, quantile, 0.025) 
  pboot[, 3] <- pboot[, 1] + 2* sdConfBoot # apply(Mboot, 1, quantile, 0.975)
  pboot[, 4] <- pboot[, 1] - 2* sdPredBoot # apply(Mboot, 1, quantile, 0.025) 
  pboot[, 5] <- pboot[, 1] + 2* sdPredBoot # apply(Mboot, 1, quantile, 0.975)
  return(pboot)
}

rfPred <- function(x, y, xnew, package = c("randomForest", "grf")){
  if (package == "randomForest"){
    mRF <- randomForest(y~x, data = data.frame(x = x, y = y))
    pRF <- predict(mRF, data.frame(x = xnew))
  } else if (package == "grf"){
    mRF <- regression_forest(X = matrix(x, ncol = 1), Y = y)
    pRF <- predict(mRF, matrix(xnew, ncol = 1))$predictions
  }
}


pRFgrf <- rfPred(x = x, y = y, xnew = xnew, package = "grf")
pbootgrf <- rfPredBoot(x = x, y = y, xnew = xnew, package = "grf")
pRFrf <- rfPred(x = x, y = y, xnew = xnew, package = "randomForest")
pbootrf <- rfPredBoot(x = x, y = y, xnew = xnew, package = "randomForest")


par(mfrow = c(3,1))
title <- paste("Confidence intervals by bootstrap (package randomForest)")
plot(x, y, main = title)
lines(xnew, pRFrf, lwd = 3, lty = "dotted")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")

lines(xnew, pbootrf[, 1], lwd = 3, col = "blue")
arrows(x0 = xnew, y0 = pbootrf[, 2], y1 = pbootrf[, 3], col = "blue", 
       code = 3, angle = 90, length = 0.05)


title <- paste("Confidence intervals by bootstrap (package grf)")
plot(x, y, main = title)
lines(xnew, pRFgrf, lwd = 3, lty = "dotted")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")

lines(xnew, pbootgrf[, 1], lwd = 3, col = "blue")
arrows(x0 = xnew, y0 = pbootgrf[, 2], y1 = pbootgrf[, 3], col = "blue", 
       code = 3, angle = 90, length = 0.05)

library(grf)
mrf <- regression_forest(X = matrix(x, ncol = 1), Y = y, num.trees = 100)
prf <- predict(mrf, matrix(xnew, ncol = 1), estimate.variance = TRUE)
title <- paste("Confidence intervals by jackknife (package grf)")
plot(x, y, main = title)
lines(xnew, prf$predictions, lwd = 3, col = "blue")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")
arrows(x0 = xnew, y0 = prf$predictions - 2*sqrt(prf$variance.estimates), 
       y1 = prf$predictions + 2*sqrt(prf$variance.estimates), 
       angle = 90, length = 0.05, code = 3, col = "violet") 


par(mfrow = c(3,1))
title <- paste("Prediction intervals by bootstrap (package randomForest)")
plot(x, y, main = title)
lines(xnew, pRFrf, lwd = 3, lty = "dotted")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")

lines(xnew, pbootrf[, 1], lwd = 3, col = "blue")
arrows(x0 = xnew, y0 = pbootrf[, 4], y1 = pbootrf[, 5], col = "blue", 
       code = 3, angle = 90, length = 0.05)

title <- paste("Prediction intervals by bootstrap (package grf)")
plot(x, y, main = title)
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")
lines(xnew, pbootgrf[, 1], lwd = 3, col = "blue")
arrows(x0 = xnew, y0 = pbootgrf[, 4], y1 = pbootgrf[, 5], col = "blue", 
       code = 3, angle = 90, length = 0.05)

title <- paste("Confidence intervals by jackknife + normal approx (package grf)")
prftrain <- predict(mrf)
sd2 <- var(prftrain$predictions - y)
sdPred <- sqrt(prf$variance.estimates[,1] + rep(sd2, n))
plot(x, y, main = title)
lines(xnew, prf$predictions, lwd = 3, col = "blue")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")
arrows(x0 = xnew, y0 = prf$predictions - 2*sdPred, 
       y1 = prf$predictions + 2*sdPred, 
       angle = 90, length = 0.05, code = 3, col = "violet") 


#Compute confidence intervals with package RFinfer
library(RFinfer)
intRF <- rfPredVar(mRF, rf.data = data.frame(x, y), pred.data = data.frame(xnew, fun(xnew)), CI=TRUE, tree.type='rf')
title <- paste(interval, "intervals by bootstrap for random forest (package RFinfer)")
plot(x, y, main = title)
lines(xnew, pRF, lwd = 3, lty = "dotted")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")
# 
lines(xnew, intRF$pred, lwd = 3, col = "blue")
arrows(x0 = xnew, y0 = pboot[, 2], y1 = pboot[, 3], col = "blue", 
       code = 3, angle = 90, length = 0.05)
arrows(x0 = x, y0 = intRF$pred - 2*sqrt(intRF$pred.ij.var), 
       y1 = intRF$pred + 2*sqrt(intRF$pred.ij.var), 
       angle = 90, length = 0.05, code = 3, col = "red") 
points(x, intRF[, 1], pch = 19, col = "red")


