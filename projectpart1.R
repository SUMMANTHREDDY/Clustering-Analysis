library(MixGHD)
library(ContaminatedMixt)
library(mvtnorm)
library(dbscan)
library(kernlab)
library(matrixStats)

set.seed(130)
data1 = rGHD(n = 200, p = 2, mu = c(10,15), alpha = c(-3,2), sigma = matrix(c(1,2,2,1),nrow = 2),
             omega = 0.2, lambda = 1.2)
data2 = rMSGHD(n = 200, p= 2, mu = c(15,20), alpha = c(-4,-2), sigma = matrix(c(1,2,2,1),nrow = 2),
               omega = c(0.4,0.3), lambda = c(0.5,3))
data3 = rCN(n = 200, mu = c(30,40), Sigma = matrix(c(20,8,8,20), ncol = 2), alpha = 0.2, eta = 12)

FinalData <- as.data.frame(rbind(data1, data2, data3))
FinalData[,3] <- c(rep(1,200),  rep(2,200), rep(3,200))
colnames(FinalData) <- c('x', 'y', 'cluster')

# Plot of simulated data
plot(FinaldData[,1:2], col = FinalData[,3],  xlim = c(-190,130), ylim = c(-100,220), pch = 20)



dbscan_sim1 <- dbscan::dbscan(FinalData[,1:2], eps = 20, minPts = 5)

table(FinalData[,3],dbscan_sim1$cluster)
ARI(FinalData[,3],dbscan_sim1$cluster)


plot(FinalData[,1:2], col = dbscan_sim1$cluster, pch = 20, xlim = c(-190,130), ylim = c(-100,220))

library(cluster)
pam.data<-pam(x=FinalData[,1:2],k=3)
plot(FinalData[,1:2], col = pam.data$cluster, pch = 20, xlim = c(-190,130), ylim = c(-100,220))
table(FinalData[,3],pam.data$cluster)
ARI(FinalData[,3],pam.data$cluster)


library(teigen)
t.out <- teigen(x=FinalData[,1:2],Gs=2:3,models="UUUU") 
plot(FinalData[,1:2], col = t.out$classification, pch = 20, xlim = c(-190,130), ylim = c(-100,220))

class.predict.t <- t.out$classification
table(FinalData[,3],class.predict.t)
ARI(FinalData[,3],class.predict.t)
