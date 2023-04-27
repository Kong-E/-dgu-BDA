## Q1
data(HouseVotes84)
str(HouseVotes84)
summary(HouseVotes84)
any(is.na(HouseVotes84))
data <- na.omit(HouseVotes84)
any(is.na(data))

## Q2
library(caret)
parts <- createDataPartition(data$Class, p=0.8, list=F)
training <- data[parts,]
testing <- data[-parts,]

## Q3
library(e1071)
data.nb <- naiveBayes(Class ~., data=training)
data.nb

options(scipen=999)
pred.prob <- predict(data.nb,
                     newdata = testing,
                     type="raw")
pred.class <- predict(data.nb,
                     newdata = testing,
                     type="class")
confusionMatrix(pred.class, as.factor(testing$Class))
# Accuracy=0.933

## Q4
data.nb.l <- naiveBayes(Class ~., data=training, laplace = 1)
data.nb.l

pred.class.l <- predict(data.nb.l,
                      newdata = testing,
                      type="class")
confusionMatrix(pred.class.l, as.factor(testing$Class))
# Accuracy=0.933