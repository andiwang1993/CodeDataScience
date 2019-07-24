library(arules)
library(arulesViz)
library(Matrix)
data(Groceries)
inspect(Groceries)##etraire toutes les RA
groceries.raw<-as.character(Groceries)

rules.all <- apriori(Groceries,parameter = list(minlen=2,support=0.01,conf=0.5))
rules.all
inspect(rules.all)
quality(rules.all)
itemFrequencyPlot(Groceries)
g_freq <- apriori(Groceries, parameter = list(minlen=2, supp=0.005, conf=0.8, target="frequent itemsets"))
plot(g_freq)
g_max <- apriori(Groceries, parameter = list(minlen=2, supp=0.005, conf=0.8, target="maximally frequent itemsets"))
plot(g_max)
g_cf<-apriori(Groceries, parameter = list(minlen=2, supp=0.005, conf=0.8, target="closed frequent itemsets"))
plot(g_freq,method="graph")
setequal(g_freq, union(g_max,g_cf))

system.time(g_max)

freq<-itemFrequency(Groceries)
max(freq)

ec_freq<-eclat(Groceries,parameter = list(minlen=2, supp=0.005, target="frequent itemsets"))
inspect(ec)
ec_max<-eclat(Groceries,parameter = list(minlen=2, supp=0.005, target="closed frequent itemsets"))

BigGr<-sample(Groceries,10000,replace = TRUE)
g_freq <- apriori(Groceries, parameter = list(minlen=2, supp=0.005, conf=0.8, target="frequent itemsets"))

