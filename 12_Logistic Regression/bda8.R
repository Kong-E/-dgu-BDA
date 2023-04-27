rm(list = ls())

## 1
library(MASS)
data <- biopsy

## 2
str(data)
summary(data)
?biopsy

## 3
data <- na.omit(data)
summary(data)

## 4
data <- data[,-1]
str(data)
levels(data$class) <- c(0,1)
str(data)
summary(data)
head(data[10], 10)

## 5
set.seed(1)
library(caret)
parts <- createDataPartition(data$class, p=0.7, list=F)
training <- data[parts,]
testing <- data[-parts,]
glm <- glm(class ~ .,
           data = training,
           family = binomial)
summary(glm)
head(fitted(glm),10)

## 6
pred <- predict(glm, newdata = testing, type = "response")
yi <- as.numeric(testing$class==1)
data.frame(pred, yi)

sqrt(sum((yi-pred)^2)/204) # RMSE
library(forecast)
accuracy(pred, yi)

confusionMatrix(as.factor(ifelse(pred>0.5,'1','0')),
                testing$class) # cutoff=0.5, accuracy=0.985
