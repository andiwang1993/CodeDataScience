library(cluster.datasets)
data(mammal.dentition)

dat_clu<-data.frame(mammal.dentition)
dat_clu<-as.matrix(dat_clu[,-1])
##reduire des dimensions a 2D
v_mat<-scale(dat_clu,scale = TRUE)
varmat<-var(v_mat)
valeurp<-eigen(varmat)$values
matrT<-eigen(varmat)$vectors[,1:2]

dat_ana<-v_mat%*%matrT
colnames(dat_ana)<-c("dim 1","dim 2")

require(graphics)
set.seed(1500)
##k-means methode
(cl <- kmeans(dat_ana, 2))
plot(dat_ana, col = cl$cluster)
points(cl$centers, col = 7:8, pch = 8)
cl2.grp1<-which(cl$cluster==1)
cl2.grp1
cl2.grp2<-which(cl$cluster==2)
cl2.grp2
## random starts do help here with too many clusters
(cl5 <- kmeans(dat_ana, 5))
plot(dat_ana, col = cl5$cluster)
points(cl5$centers, col = 1:5, pch = 8)
print(cl5)
cl5.grp1<-which(cl5$cluster==1)
cl5.grp1
cl5.grp2<-which(cl5$cluster==2)
cl5.grp2
cl5.grp3<-which(cl5$cluster==3)
cl5.grp3
cl5.grp4<-which(cl5$cluster==4)
cl5.grp4
cl5.grp5<-which(cl5$cluster==5)
cl5.grp5

### k-medoids
library(clue)
library(cluster)

m2 <- pam(dat_ana,2)
plot(dat_ana, col = m2$cluster)
points(m2$medoids, col = 7:8, pch = 8)
m2.grp1<-which(m2$cluster==1)
m2.grp1
m2.grp2<-which(m2$cluster==2)
m2.grp2

m5 <- pam(dat_ana,5)
plot(dat_ana, col = m5$cluster)
points(m5$medoids, col = 1:5, pch = 8)
m5.grp1<-which(m5$cluster==1)
m5.grp1
m5.grp2<-which(m5$cluster==2)
m5.grp2
m5.grp3<-which(m5$cluster==3)
m5.grp3
m5.grp4<-which(m5$cluster==4)
m5.grp4
m5.grp5<-which(m5$cluster==5)
m5.grp5


### hierarchical


d<-dist(dat_ana)
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
for (i in 1:32) z <- rbind(z,c)
c <- c(1, 0)
for (i in 1:33) z <- rbind(z,c)
set.seed(200)
EM1 <- me(model = 'EII', data=dat_ana, z = z)

plot(dat_ana,col=round(EM1$z[,1] + 3*EM1$z[,2]))


EM1
EM1$z

# la couleur du point est calculee en fonction de la probabilte d'appartenance.

round(EM1$z[,1] + 3*EM1$z[,2])
# # La majorite  ont une probabiblite proche de 1, mais il y aussi d'autre valeurs
# EM1$z[15,]
# dat_ana[15,]
# 
# EM1$z[76,]
# dat_ana[76,]

library(fpc)
db_dat_ana <- dbscan(dat_ana,eps=0.2,MinPts=3)
db_dat_ana
db_dat_ana$cluster
plot(dat_ana, col = db_dat_ana$cluster + 1)


############knn##########

z <- c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1,1, 1, 1, 1, 1,1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ,2, 2, 1, 2, 2, 2, 2, 2)

donnees<-cbind(dat_ana,z)
plot(dat_ana[,1:2], col = z)

# on pr¨¦pare un sous-ensemble d'apprentissae et un autre de test
n <- nrow(dat_ana)

I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on pr¨¦pare les donn¨¦es : on construit le classifieur K-NN
# pour les valeurs num¨¦rique et on extrait explicitement la classe 
# ¨¤ predire

cl <- donnees[I,3]

dlrn <- donnees[I,1:2]
dtest <- donnees[J,1:2]

library (class)

mknn1 <- knn(dlrn, dtest,cl, k=1)
mknn1
###matrice de confusion
table(mknn1, donnees[J,3])


mknn3 <- knn(dlrn, dtest,cl, k=3)
mknn3
###matrice de confusion
table(mknn3, donnees[J,3])

mknn7 <- knn(dlrn, dtest,cl, k=7)
mknn7
###matrice de confusion
table(mknn7, donnees[J,3])

mknn11 <- knn(dlrn, dtest,cl, k=11)
mknn11
###matrice de confusion
table(mknn11, donnees[J,3])

# validation crois¨¦e

train <- donnees[,1:2]
cl <- donnees[,3]
model <- knn.cv(train,cl,k=5)
model

###matrice de confusion
table(cl,model)

plot(dat_ana,col=as.numeric(model))
###############la methode de K-Moyenne


#fonction pour calculer distande eucludienne
distance<- function(vec1,vec2){
  distance = sqrt(sum((vec1-vec2)^2))
  return (distance)
}

##fonction pour etabir des clusters initiales
cent <- function(data,k){
  n = ncol(data)
  centroids = matrix(data = NA, nrow = k, ncol = n, byrow = FALSE,dimnames = NULL)
  for(j in 1:n){
    minJ = min(data[,j])
    interJ = max(data[,j]) - minJ
    #valeurs aleatoires
    centroids[,j] = minJ + interJ *runif(k)
  }
  return (centroids)
} 
kMeans <- function(data,k,distance, cent){
  m = nrow(data)
  #matrice pour des clusters et erreurs, initialise cluster avec des donnees aleatoire
  clusterAssment = cbind(sample(1:k,m,replace = TRUE),matrix(data = 0,  nrow = m , ncol = 1,byrow = FALSE,dimnames = NULL))
  
  centroids =cent(data,k)
  colnames(centroids) = colnames(data)
  
  
  changed = TRUE
  #si changed = true, the centroids et des clusters restent changes, sinon finir boucle
  while (changed) {
    changed = FALSE
    for(i in 1: m){
      minDist = Inf#une valeur grande pour commencer, et il va change suivant
      minIndex = -1
      for(j in 1:k){
        distJI = distance(data[i,],centroids[j,])
        if(is.na(distJI)) distJI = Inf
        if(distJI < minDist){
          minDist = distJI
          minIndex = j
        }
      }
      if(clusterAssment[i,1] != minIndex){
        changed = TRUE
        clusterAssment[i,1] = minIndex 
      } 
      clusterAssment[i,2] = minDist^2
    }
    
    for(cent in 1:k){
      ptsCluster = data[which(clusterAssment[,1] == cent),]
      centroids[cent,] = apply(ptsCluster,2,mean)
    }
  }
  
  out = list(clusterAssment = clusterAssment ,centroids = centroids)
  return (out)
}

output = kMeans(dat_ana,2,distance,cent)

plot(dat_ana,col = output$clusterAssment[,1]) 
points(output$centroids[,1],output$centroids[,2],col = 1:2,pch=8)

output$clusterAssment[,1] #cluster

