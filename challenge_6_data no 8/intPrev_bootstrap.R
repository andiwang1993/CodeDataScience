n <- 20
sigma <- 0.05
x <- seq(0, 1, length.out = n)
X <- cbind(rep(1,n), x)
beta <- matrix(c(1, 0.5), ncol = 1)
set.seed(0)
y <- X%*%beta + rnorm(n, mean = 0, sd = sigma)

plot(x, y) 
abline(a = beta[1], b = beta[2], lty = "dotted")

df <- data.frame(x = X, y = y)
m <- lm(y~x, data = df)
abline(m, col = "blue")

nNew <- 20
xnew = x; #runif(nNew)
p <- predict(m, newdata = data.frame(x = xnew), interval = "pred", level = 0.95)
arrows(x0 = xnew, y0 = p[, 2], y1 = p[, 3], col = "red", code = 3, angle = 90, length = 0.05)
points(xnew, p[, 1], pch = 19)


# par bootstrap sur les donnees
nboot <- 1000
Mboot <- matrix(NA, nrow = n, ncol = nboot)
option <- 'non parametric'  # or 'non parametric'

for (i in 1:nboot){
  index <- sample(n, replace = TRUE)
  mboot <- lm(y~x, data.frame(x = x[index], y = y[index]))
  Mboot[, i] <- predict(mboot, newdata = data.frame(x = x))
  if (option=="parametric"){
    Mboot[, i] <- Mboot[, i] + rnorm(n, 0, sd(mboot$residuals))
  } else if (option == "non parametric"){
    Mboot[, i] <- Mboot[, i] + sample(mboot$residuals, size = n, replace = TRUE)
  } else stop()
}

pboot <- matrix(NA, nrow = n, ncol = 3)
pboot[, 1] <- rowMeans(Mboot)
pboot[, 2] <- apply(Mboot, 1, quantile, 0.025) 
pboot[, 3] <- apply(Mboot, 1, quantile, 0.975)
arrows(x0 = xnew, y0 = pboot[, 2], y1 = pboot[, 3], col = "blue", code = 3, angle = 90, length = 0.05)
points(xnew, pboot[, 1], pch = 19)
legend('topleft', legend = c("Exact", paste("Bootstrap (", option, ")", sep = "")), 
       col = c("grey", "blue"), lty = c(1,1))


