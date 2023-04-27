data("PimaIndiansDiabetes2")
data <- PimaIndiansDiabetes2
str(data)
summary(data)

# 1
data$glucose[is.na(data$glucose)] <- median(data$glucose, na.rm = T)
data$pressure[is.na(data$pressure)] <- median(data$pressure, na.rm = T)
data$triceps[is.na(data$triceps)] <- median(data$triceps, na.rm = T)
data$insulin[is.na(data$insulin)] <- median(data$insulin, na.rm = T)
data$mass[is.na(data$mass)] <- median(data$mass, na.rm = T)
summary(data)

# 2
set.seed(1)
train.index <- sample(row.names(data), 0.6*dim(data)[1])
valid.index <- sample(setdiff(row.names(data), train.index), 0.2*dim(data)[1])
test.index <- setdiff(row.names(data), union(train.index,valid.index))

train.df <- data[train.index,]
valid.df <- data[valid.index,]
test.df <- data[test.index,]

plot(glucose ~ pressure, data=train.df, pch=ifelse(train.df$diabetes=="pos",1,3))

train.norm.df <- train.df
valid.norm.df <- valid.df
test.norm.df <- test.df
data.norm <- data

library(caret)
norm.values <- preProcess(train.df[,1:8], method=c("center","scale"))

train.norm.df[, 1:8] <- predict(norm.values, train.df[,1:8])
valid.norm.df[, 1:8] <- predict(norm.values, valid.df[,1:8])
test.norm.df[, 1:8] <- predict(norm.values, test.df[,1:8])
data.norm[, 1:8] <- predict(norm.values, data[,1:8])

head(train.norm.df)
head(valid.norm.df)
head(test.norm.df)

train.norm.df.x <- train.norm.df[, 1:8]
train.norm.df.y <- train.norm.df[, 9]
valid.norm.df.x <- valid.norm.df[, 1:8]
valid.norm.df.y <- valid.norm.df[, 9]
test.norm.df.x <- test.norm.df[, 1:8]
test.norm.df.y <- test.norm.df[, 9]

# 3
library(FNN)
knn.pred <- knn(train.norm.df.x, valid.norm.df.x,
          cl = train.norm.df.y, k = 3)

accuracy <- confusionMatrix(knn.pred,
                            as.factor(valid.norm.df.y))$overall[1]

# 4
library(caret)

accuracy.df <- data.frame(k=seq(1,460,1),accuracy=rep(0,460))
head(accuracy.df)
tail(accuracy.df)

for(i in 1:460) {
  knn.pred <- knn(train = train.norm.df.x, 
                  test = valid.norm.df.x,
                  cl = train.norm.df.y,
                  k = i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred,
                                      as.factor(valid.norm.df.y))$overall[1]
}
accuracy.df
summary(accuracy.df$accuracy)
accuracy.df[round(accuracy.df$accuracy,digits = 4)==0.8366,]
accuracy.df[95,]

# 5
knn.pred <- knn(train.norm.df.x, 
                test.norm.df.x,
                cl = train.norm.df.y, 
                k = 95)
result <- confusionMatrix(knn.pred,
                as.factor(test.norm.df.y))$table
result
accuracy <- confusionMatrix(knn.pred,
                            as.factor(test.norm.df.y))$overall[1]
accuracy
