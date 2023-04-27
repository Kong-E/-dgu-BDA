## Q1-1
library(rpart)
data(stagec)
data <- stagec
str(data)

test.df <- data[145:146,-8]
train.df <- data[1:144,]

## Q1-2
my_control <- rpart.control(xval = 10, cp = 0, 
                            minsplit = 4)
tree_model <- rpart(ploidy~.,method = "class",
                    control = my_control, data = train.df)
tree_model

library(rpart.plot)
rpart.plot(tree_model)

## Q1-3
printcp(tree_model)
pruned_model <- prune.rpart(tree_model, cp = 0.013)
rpart.plot(pruned_model)

## Q1-4
predict(pruned_model, newdata = test.df, type = "class")

## Q2-1
install.packages("TH.data")
library("TH.data")
data(bodyfat)
data2 <- bodyfat
str(data2)
?bodyfat 
# DEXfat is the dependent variable.

## Q2-2
my_control2 <- rpart.control(xval = 10, cp = 0,
                             minsplit = 8)
tree_model2 <- rpart(DEXfat~., method = "anova",
                     control = my_control2, data = data2)
tree_model2
rpart.plot(tree_model2)

## Q2-3
printcp(tree_model2)
pruned_model2 <- prune.rpart(tree_model2, cp = 0.00085)
rpart.plot(pruned_model2)
