require(graphics)

# un exemple avec des valeurs generees aleatoirement 

# garder ce germe afin d'avoir des points interessants
set.seed(1500)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 7:8, pch = 8, cex=2)


## random starts do help here with too many clusters
(cl5 <- kmeans(x, 5))
plot(x, col = cl5$cluster)
points(cl5$centers, col = 1:5, pch = 8)

### k-medoids
library(clue)
library(cluster)

m2 <- pam(x,2)
plot(x, col = m2$cluster)

m5 <- pam(x,5)
plot(x, col = m5$cluster)
points(m5$centers, col = 1:5, pch = 8)


### hierarchical

xx <- rbind(x[1:10,], x[91:100,])

d <- dist(xx)

hc1 <- hclust(d,"ward")
plot(hc1)

hc2 <- hclust(d,"ave")
plot(hc2)

hc3 <- hclust(d,"median")
plot(hc3)

hc4 <- hclust(d,"centroid")
plot(hc4)

# appel de la methode EM
library(mclust)
 # on doit preparer un vecteur de distribution prealable (les valeurs pi de l'algo)
c <- c(0, 1)
z <- c
for (i in 1:49) z <- rbind(z,c)
c <- c(1, 0)
for (i in 1:50) z <- rbind(z,c)
set.seed(200)
EM1 <- me(model = 'EII', data=x, z = z)

plot(x,col=round(EM1$z[,1] + 3*EM1$z[,2]))

EM1
EM1$z

# la couleur du point est calculee en fonction de la probabilte d'appartenance.

round(EM1$z[,1] + 3*EM1$z[,2])
# La majorite  ont une probabiblite proche de 1, mais il y aussi d'autre valeurs
EM1$z[15,]
x[15,]

EM1$z[76,]
x[76,]

library(fpc)
db_x <- dbscan(x,eps=0.2,MinPts=3)
db_x
db_x$cluster
plot(x, col = db_x$cluster + 1)

