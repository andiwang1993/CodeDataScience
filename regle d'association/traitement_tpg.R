library(readr)
library(clue)
library(cluster)
library(class)
library(arules)
library(arulesViz)

tpg <- read_delim("tpg.csv", delim=";")

View(tpg)

summary(tpg)

#--------------------------------------------
#------ on applique les clustering ----------
#--------------------------------------------
dd<-tpg

convert_classe <- function(x)
{
  x <- gsub(pattern = "F" , replacement = "1" , x, fixed = TRUE)
  x <- gsub(pattern = "M" , replacement = "2" , x, fixed = TRUE)
  return(x)
}

convert_cheveux <- function(x)
{
  x <- gsub(pattern = "C" , replacement = "0" , x, fixed = TRUE)
  x <- gsub(pattern = "ML" , replacement = "1" , x, fixed = TRUE)
  x <- gsub(pattern = "L" , replacement = "2" , x, fixed = TRUE)
  return(x)
}

classe <- convert_classe(dd[,3])
cheveux <- convert_cheveux(dd[,2])

dd <- data.frame(Pointure = dd[,1], Cheveux = cheveux, Genre = classe)

pairs(dd, col=dd[,3])

plot(dd[,1:2], col=dd[,3])
cl2 <- kmeans(dd[,1:2], 2)
cl2
cl2$cluster
table(cl2$cluster, dd[,3])
cl2$centers

cl_pam <- pam(dd[,1:2], 2)
cl_pam
cl_pam$medoids
table(cl_pam$clustering, dd[,3])

#--------------------------------------
# on classifie avec K-nn
#--------------------------------------

train <- dd[,1:2]
cl <- dd[,3]

#----- pour 5 voisins
model5 <- knn.cv(train,cl,k=5)
model5
table(model5, dd[,3])
# 1 élémént mal classé ! 

#---- pour 3 voisins
model3 <- knn.cv(train,cl,k=3)
model3
table(model3, dd[,3])


## --- usage des RA ------

tt <- tpg

# on ne peut pas traiter les données tpg en état, il faut les factoriser

tt <- as.data.frame(sapply(tt, factor, simplify=FALSE))

rules.all <- apriori(tt)
rules.all
inspect(rules.all)
quality(rules.all)

rules <- apriori(tt, control = list(verbose=F),
                 parameter = list(minlen=2, supp=2/32, conf=0.8),
                 appearance = list(rhs=c("Genre=M", "Genre=F"),
                                   default="lhs"))
rules
inspect(rules)

