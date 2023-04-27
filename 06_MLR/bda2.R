state.df <- data.frame(state.x77)

#Q1
head(state.df, 3)

#Q2
selected.var <- c(5,1,3,2,7)
stat.df <- state.df[selected.var]
str(stat.df)

#Q3
# partition data
set.seed(1)
train.index <- sample(c(1:50), 30)
train.df <- stat.df[train.index,]
valid.df <- stat.df[-train.index,]

stat.lm <- lm(Murder~., data = train.df)
coef(stat.lm)
summary(stat.lm)

# MSE
stat.lm.pred <- predict(stat.lm, valid.df)
errors <- unname((valid.df$Murder - stat.lm.pred) ^ 2)
sum(errors) / length(Murder)

#Q4
stat.lm.step <- step(stat.lm, direction = "both")
coef(stat.lm.step)
summary(stat.lm.step)

# MSE
stat.lm.step.pred <- predict(stat.lm.step, valid.df)
errors <- unname((valid.df$Murder - stat.lm.step.pred) ^ 2)
sum(errors) / length(Murder)

#Q5
data.frame("Predicted" = stat.lm.pred,
           "Actual" = valid.df$Murder,
           "Residual" = valid.df$Murder - stat.lm.pred)
data.frame("Predicted" = stat.lm.step.pred,
           "Actual" = valid.df$Murder,
           "Residual" = valid.df$Murder - stat.lm.step.pred)
# 모델의 평가측도 계산
library(forecast)
#accuracy(stat.lm$fitted.values, train.df$Murder)
accuracy(stat.lm.pred, valid.df$Murder)

#accuracy(stat.lm.step$fitted.values, train.df$Murder)
accuracy(stat.lm.step.pred, valid.df$Murder) 

######################
# not partition
lm <- lm(Murder~., data=stat.df)
summary(lm)
lm.pred <- predict(lm)

lm.step <- step(lm, direction = "both")
summary(lm.step)
lm.step.pred <- predict(lm.step)

data.frame("Predicted" = lm.pred,
           "Actual" = stat.df$Murder,
           "Residual" = stat.df$Murder - lm.pred)
data.frame("Predicted" = lm.step.pred,
           "Actual" = stat.df$Murder,
           "Residual" = stat.df$Murder - lm.step.pred)

library(forecast)
accuracy(lm.pred, stat.df$Murder)
accuracy(lm.step.pred, stat.df$Murder)
