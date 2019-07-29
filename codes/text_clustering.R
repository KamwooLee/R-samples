# Text as Data
# Author: Kamwoo Lee
# Last modified: 10/19/2016
#

library(rvest)
library(tm)
library(stringr)
library(XML)
library(cluster) 
library(proxy)
library(dplyr)
library(ggplot2)



##############################################################
# 1. Choose a Corpus & 2. Explain your preprocessing choices #
##############################################################

load("ecorpt.RData")
# This code uses the result of analysis in the previous problem set.



##################
# 3. Clustering  #
##################

# Start with previous stemmed corpus, make a DocumentTermMatrix
eco_report.DTM <- DocumentTermMatrix(eco_report.corpus.stem.comp, control=list(bounds=list(global=c(3,Inf))))

# Convert it to a matrix for analysis
eco_report.Mat <- as.matrix(eco_report.DTM)


# 3.1. Hiearchical Clustering

# Cosine similarity, Ward's linkage
# Create distance matrix (dist) and implement linkage method (hclust)
eco_report.cd <- dist(eco_report.Mat, method="cosine")
hc.cd.ward <- hclust(eco_report.cd, method = "ward.D2")

# Plot dendrogram
plot(hc.cd.ward, main="Cosine Distance, Ward's Method",
     labels = paste(eco_report.df$president, eco_report.df$year), xlab="", sub="", cex=.6)
# Add helpful rectangles around clusters
rect.hclust(hc.cd.ward, k=12, border="red")

# Make cluster assignment for 12 groups
hc.12 <- cutree(hc.cd.ward, k=12) 
table(hc.12)

# Cluster Labeling
# Recall dcMat is the matrix version of dcDTM
eco_report.DF <- as.data.frame(eco_report.Mat) # coerce to dataframe
eco_report.DF <- data.frame(eco_report.DF, hc.12) # append clusters

# Obtain most highly weighted words in each cluster
eco_report.List <- split(eco_report.DF, eco_report.DF$hc.12) # split into lists by cluster
# apply function to each list: sum columns/weights, sort, save top 10
topwords <- lapply(eco_report.List, function(x) { 
  sort(apply(x[,-ncol(x)], 2, sum), decreasing=TRUE)[1:10]
})
topwords





# 3.2. Kmeans clustering with k=6
# Normalize the tf.idf matrix (for Euclidean distance)
eco_report.MatN <- eco_report.Mat / apply(eco_report.Mat, MARGIN=1, FUN = function(x) sum(x^2)^0.5)

# How many clusters?
# Plot the within group sum of squares: look for "elbow"
ssPlot <- function(data, maxCluster) { # 2 arguments: data and maximum k
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i-1] <- kmeans(data, centers=i)$tot.withinss
  }
  plot(2:maxCluster, SSw, type="b", xlab="# of Clusters", ylab="Within groups SS")
}
set.seed(123)
ssPlot(eco_report.MatN, 20)

# cluster with 6 groups
km <- kmeans(eco_report.MatN, 6, iter.max=25, nstart=5)
km.cluster <- km$cluster
table(km.cluster)

# Add cluster membership to data frame
eco_report.DF[,"km.cluster"] <- as.factor(km.cluster)

# Most highly weighted words in each cluster
eco_report.List <- split(eco_report.DF, eco_report.DF$km.cluster) # split into lists by cluster
# apply function to each list: sum columns/weights, sort, save top 10
topwords <- lapply(eco_report.List, function(x) { 
  sort(apply(x[,-ncol(x)], 2, sum), decreasing=TRUE)[1:10]
})
topwords





# 3.3. Kmeans clustering with k=12
# Normalize the tf.idf matrix (for Euclidean distance)
eco_report.MatN <- eco_report.Mat / apply(eco_report.Mat, MARGIN=1, FUN = function(x) sum(x^2)^0.5)

# How many clusters?
# Plot the within group sum of squares: look for "elbow"
ssPlot <- function(data, maxCluster) { # 2 arguments: data and maximum k
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i-1] <- kmeans(data, centers=i)$tot.withinss
  }
  plot(2:maxCluster, SSw, type="b", xlab="# of Clusters", ylab="Within groups SS")
}
set.seed(2222)
ssPlot(eco_report.MatN, 20)

# cluster with 12 groups
km <- kmeans(eco_report.MatN, 12, iter.max=25, nstart=5)
km.cluster <- km$cluster
table(km.cluster)

# Add cluster membership to data frame
eco_report.DF[,"km.cluster"] <- as.factor(km.cluster)

# Most highly weighted words in each cluster
eco_report.List <- split(eco_report.DF, eco_report.DF$km.cluster) # split into lists by cluster
# apply function to each list: sum columns/weights, sort, save top 10
topwords <- lapply(eco_report.List, function(x) { 
  sort(apply(x[,-ncol(x)], 2, sum), decreasing=TRUE)[1:10]
})
topwords





######################
# 4. Interpretation  #
######################
# Hiearchical Clustering gives the most intuitive result.
# Since there are 12 president in this corpus, 12 clusters are reasonable choice.
# It is noticible that economic report of George W. Buch in 2002 stands out without a cluster.
# It can be conjectured that this report is right after 9/11 terrist attacks.
# So, even though it is an economic report there are many messages that have to do with terror or national security.
# This clustring shows larger gaps between older reports and newer reports.
# For future analysis, we could delve into the reason of the gaps.
# Such as, whether there are meaningful differnece in financial terms or in report styles.



save.image("../ecorpt.RData")