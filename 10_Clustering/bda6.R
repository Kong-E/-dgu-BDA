setwd("C:/Users/sy/Documents/bda")
data <- read.table("wine.csv", header=T, sep=",")

## 1
str(data)
head(data)
y <- data[,1]
x <- data[,-1]

## 2
x.norm <- data.frame(sapply(x, scale))
head(x.norm)

## 3
d.norm <- dist(x.norm, method="euclidean")
hc1_single <- hclust(d.norm, method="single")
plot(hc1_single, hang=-1, ann=FALSE)

hc2_complete <- hclust(d.norm, method="complete")
plot(hc2_complete, hang=-1, ann=FALSE)

hc3_average <- hclust(d.norm, method="average")
plot(hc3_average, hang=-1, ann=FALSE)

## 4
# 1) SSE
SSE <- c()

for (i in 1:20){
  set.seed(1)
  kmeans_cluster <- kmeans(x.norm, i)
  SSE[i] <- kmeans_cluster$tot.withinss
}

plot(c(1:20), SSE, type="b")
# k=3 

# 2) Silhouette
library("cluster")

km2 <- kmeans(x.norm, 2)
sil2 <- silhouette(km2$cluster, dist(x.norm))
plot(sil2)

km3 <- kmeans(x.norm, 3)
sil3 <- silhouette(km3$cluster, dist(x.norm))
plot(sil3)

km4 <- kmeans(x.norm, 4)
sil4 <- silhouette(km4$cluster, dist(x.norm))
plot(sil4)

km5 <- kmeans(x.norm, 5)
sil5 <- silhouette(km5$cluster, dist(x.norm))
plot(sil5)

# k=3

## 5
set.seed(1122334455)
km <- kmeans(x.norm, 3)
comp <- data.frame(km$cluster, y)
comp

## 6
y.i <- NULL
y.i[y=="A"] <- 2
y.i[y=="B"] <- 1
y.i[y=="C"] <- 3

library(forecast)
accuracy(km$cluster, y.i)
