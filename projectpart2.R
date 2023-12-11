library(mvtnorm)

# Set seed for reproducibility
set.seed(123)

# Generate four means for the clusters
means <- matrix(c(-2, 2, 4, -4, 8, 10, -2.5, 3), ncol = 2)

# Generate covariance matrices for each cluster
covs <- list()
for (i in 1:4) {
  covs[[i]] <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
}

# Generate data for each cluster
n <- 200


d1 <- rbind( rmvnorm(n, means[1,], covs[[1]]))
d2 <- rbind( rmvnorm(n, means[2,], covs[[2]]))
d3 <- rbind( rmvnorm(n, means[3,], covs[[3]]))
d4 <- rbind( rmvnorm(n, means[4,], covs[[4]]))


combineddata <- as.data.frame(rbind(d1, d2, d3,d4))
combineddata[,3] <- c(rep(1,200),  rep(2,200), rep(3,200),rep(4,200))
colnames(combineddata) <- c('x', 'y', 'cluster')

plot(combineddata[,1:2], col = combineddata[,3],  xlim = c(-10,10), ylim = c(-10,15), pch = 20)











# Load the DBSCAN library
library(dbscan)

# Run DBSCAN
db <- dbscan(combineddata[,1:2], eps = 1.2, minPts = 8)

# Plot the results
plot(combineddata[,1:2], col = db$cluster, pch = 20, xlim = c(-10,10), ylim = c(-10,15))
table(combineddata[,3],db$cluster)
ARI(FinalData[,3],dbscan_sim1$cluster)


plot(combineddata[,1:2], col = db$cluster, pch = 20, xlim = c(-10,10), ylim = c(-10,15))

library(cluster)
pam.data<-pam(x=combineddata[,1:2],k=4)
plot(combineddata[,1:2], col = pam.data$cluster, pch = 20, xlim = c(-10,10), ylim = c(-10,15))
s.vals <- silhouette(x=pam.data$cluster,dist=dist(combineddata[,1:2],method="manhattan")) # computes the silhouette values
plot(s.vals,main="Silhouette Plot of PAM Solution",col="blue") # gives a plot of the silhouette values


table(combineddata[,3],pam.data$clustering)
ARI(combineddata[,3],pam.data$cluster)


library(teigen)
tc.out <- teigen(x=combineddata[,1:2],Gs=2:5,models="UUUU") 
plot(combineddata[,1:2], col = tc.out$classification, pch = 20, xlim = c(-10,10), ylim = c(-10,15))

class.predict.t <- tc.out$classification
table(combineddata[,3],class.predict.t)
ARI(combineddata[,3],class.predict.t)



library(ContaminatedMixt)
# Fit a contaminated mixture model to the data
fit1 <- CNmixt(combineddata[,1:2], G = 4)
predicted_clusters <- fit1$classification
true_clusters <- combineddata[,3]
plot(fit1,pch = 20, xlim = c(-10,10), ylim = c(-10,15))





###########################second simulated data######################

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
plot(FinalData[,1:2], col = FinalData[,3],  xlim = c(-190,130), ylim = c(-100,220), pch = 20)
dbscan_sim1 <- dbscan::dbscan(FinalData[,1:2], eps = 5, minPts = 4)
table(FinalData[,3],dbscan_sim1$cluster)
ARI(FinalData[,3],dbscan_sim1$cluster)


plot(FinalData[,1:2], col = dbscan_sim1$cluster, pch = 20, xlim = c(-190,130), ylim = c(-100,220))

library(cluster)
pam.data<-pam(x=FinalData[,1:2],k=3)
plot(FinalData[,1:2], col = pam.data$cluster, pch = 20, xlim = c(-190,130), ylim = c(-100,220))
table(FinalData[,3],pam.data$cluster)
ARI(FinalData[,3],pam.data$cluster)


library(teigen)
t.out <- teigen(x=FinalData[,1:2],Gs=2:4,models="UUUU") 
plot(FinalData[,1:2], col = t.out$classification, pch = 20, xlim = c(-190,130), ylim = c(-100,220))

class.predict.t <- t.out$classification
table(FinalData[,3],class.predict.t)
ARI(FinalData[,3],class.predict.t)






install.packages(c("ContaminatedMixt"))
library(ContaminatedMixt)
# Fit a contaminated mixture model to the data
fit <- CNmixt(FinalData[,1:2], G = 3)
predicted_clusters <- fit$classification
true_clusters <- FinalData[,3]
plot(fit,pch = 20, xlim = c(-190,130), ylim = c(-100,220))
ari <- adjustedRandIndex(predicted_clusters, true_clusters)
summary(fit)
agree(fit,FinalData[,3])`






