library(neuralnet)
n <- names(x)
f <- as.formula(paste("yGas ~", paste(n[!n %in% "yGas"], collapse = " + ")))
netGas <- neuralnet(f, data = as.matrix.data.frame(x),hidden = 1, threshold = 0.01)
print(netGas)
plot(netGas)



pred<-compute(net,datasets[-trainingindexes])
plot(datasets[-trainingindexes],pred$net.result,col="red")

