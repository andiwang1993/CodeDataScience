## generate the data

N <- 30
set.seed(0)
x <- runif(N, min=-10, max=40)
fun <- function(x){
  0.5*pmax(x-20, 0) + pmax(16-x, 0)
}

y <- fun(x) + 2*rnorm(N)
plot(x, y)
xnew <- seq(-10, 40, length.out = 200)
lines(xnew, fun(xnew), lty="dotted")


## estimation d'un arbre de regression
library(tree)
mtree <- tree(y~x, data = data.frame(x=x, y=y))
p <- predict(mtree, data.frame(x = xnew))
plot(mtree); text(mtree)
plot(x, y)
lines(xnew, p, col = "blue")


## effect of boostrapping on the tree
treeBoot <- function(x, y, control = tree.control(length(x), ...), plot = TRUE, ...){
  N <- length(x)
  indBoot <- sample(1:N, N, replace = TRUE)
  xBoot <- x[indBoot]
  yBoot <- y[indBoot]
  mtreeBoot <- tree(y~x, data = data.frame(x=xBoot, y=yBoot),
                    control = control)
  pBoot <- predict(mtreeBoot, data.frame(x = xnew))
  if (plot) {
    points(x, y, pch = 19, cex = 0.5)
    col <- sample(2:8, 1)
    points(xBoot, yBoot, col = col, pch = 19, cex = 0.5)
    lines(xnew, pBoot, col = col, ...)
  }
  invisible(pBoot)
}

plot(x, y, cex = 0.5)
treeBoot(x, y)
treeBoot(x, y, control = tree.control(nobs = length(x), mincut = 1, minsize = 2))

# bagging trees
B <- 500
pBoot <- matrix(NA, length(xnew), B)

plot(x, y)
for (b in 1:B){
  pBoot[, b] <- treeBoot(x, y, plot = TRUE, 
                         control = tree.control(nobs = length(x), 
                                                mincut = 1, minsize = 2),
                         lty = "dotted", lwd = 0.2)
}
pBagging <- rowMeans(pBoot)
lines(xnew, pBagging, lwd = 3, col = "blue")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")


library(randomForest)  # of course here, feature sampling is useless
mRF <- randomForest(y~x, data = data.frame(x = x, y = y))
plot(x, y)
pRF <- predict(mRF, data.frame(x = xnew))
lines(xnew, pRF, lwd = 3, col = "blue")
lines(xnew, fun(xnew), lwd = 3, col = "red", lty = "dashed")

library(bootstrap)
perc95 <- function(x){quantile(x, .95)}
theta <- function(x){mean(x)} 


results <-  bootstrap(x,100,theta, func=perc95)    
###summary(results)
plot(results)

