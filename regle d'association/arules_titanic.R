library(arules)
library(arulesViz)

df <- as.data.frame(Titanic)
head (df)
class(df)

titanic.raw <- NULL
for(i in 1:4) {
 titanic.raw <- cbind(titanic.raw, rep(as.character(df[,i]),
                                          df$Freq))
}

titanic.raw <- as.data.frame(titanic.raw)
names(titanic.raw) <- names(df)[1:4]

head(titanic.raw)
dim(titanic.raw)

rules.all <- apriori(titanic.raw)
rules.all
inspect(rules.all)
quality(rules.all)

rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                 default="lhs"))
rules
inspect(rules)
plot(rules)
plot(rules, method="graph")

quality(rules) <- round(quality(rules), digits=3)
inspect(rules)
plot(rules)
plot(rules, method="graph")

image(Titanic)

Titanic_FIS <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8, target="frequent itemsets"))
Titanic_MFIS <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8, target="maximally frequent itemsets"))

Titanic_CFIS <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8, target="closed frequent itemsets"))

inspect(Titanic_FIS)
inspect(Titanic_MFIS)
inspect(Titanic_CFIS)

setequal(Titanic_CFIS, union(Titanic_MFIS,Titanic_CFIS))
setequal(Titanic_FIS, union(Titanic_MFIS,Titanic_CFIS))

Titanic_EIS <- eclat(titanic.raw, parameter = list(minlen=2, supp=0.005, target="frequent itemsets"))
Titanic_MEIS <- eclat(titanic.raw, parameter = list(minlen=2, supp=0.005, target="maximally frequent itemsets"))

Titanic_CEIS <- eclat(titanic.raw, parameter = list(minlen=2, supp=0.005, target="closed frequent itemsets"))

setdiff(Titanic_CEIS,Titanic_CFIS)
setdiff(Titanic_EIS,Titanic_FIS)

Titanic_FIS
Titanic_MFIS
inspect(Titanic_MEIS)
