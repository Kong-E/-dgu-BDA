# MLR 
rm(list=ls())

attach(mtcars)

head(mtcars)
str(mtcars)
plot(disp, mpg, col="blue", pch=20)

# Simple Linear Regression
model <- lm(mpg ~ disp, data = mtcars)
coef(model)

# Prediction value of Training data
y_preds <- predict(model)
abline(model)

#MSE
errors <- unname((mpg - y_preds)^2)
sum(errors) / length(mpg)

########################
## 경사하강법 알고리즘
########################
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((2 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((2 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    if(iterations %% 50000==0){
      cat("intercept:", c,  "slope:", m, "MSE:", MSE_new, "\n")
    }
    
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m)
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if (iterations > max_iter) {
      abline(c, m)
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}

# Run the function
gradientDesc(x=disp,
             y=mpg,
             learn_rate=0.00001,
             conv_threshold = 0.001,
             n=32,
             max_iter = 250000)

#MLR 모델의 적합 (도요타코롤라 중고자동차 가격 예측)

car.df <- read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000,]
# select variables for regression
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

#partition data
set.seed(1)
train.index <- sample(c(1:1000),600)
train.df<-car.df[train.index,selected.var]
valid.df<-car.df[-train.index,selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm<-lm(Price~.,data=train.df)

# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

# validation set (20 cases)의 예측값 및 성능

library(forecast)

# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm, valid.df) 
# valid 셋의 x 값을 car.lm 모델에 집어넣어본다

options(scipen = 999, digits = 1)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20],
           "Actual" = valid.df$Price[1:20],
           "Residual"=some.residuals)

options(scipen = 999, digits = 3)

# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred,valid.df$Price)

#Histogram of validation errors
library(forecast)
car.lm.pred <- predict(car.lm,valid.df)
all.residuals <- valid.df$Price - car.lm.pred

hist(all.residuals,
     breaks=25,
     xlab="Residuals",main="")

# 전역탐색 (Exhausive search): Best Subset Selection

# use regsubsets() in package leaps to run an exhaustive search.
library(leaps)
search <- regsubsets(Price~.,
                     data=train.df,
                     nbest=1,
                     nvmax=dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
sum
sum$rsq
sum$adjr2

# Backward Selection 방법 (step()의 이용)
# regsubsets()를 사용할 수도 있다.

# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both"
car.lm.step <- step(car.lm, direction="backward")
summary(car.lm.step) # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# Forward Selection 방법

# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)

# use step() to run forward regression.
car.lm.step <- step(car.lm.null,
                    scope=list(lower=car.lm.null,
                               upper=car.lm),
                    direction = "forward")
summary(car.lm.step) # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# Stepwise Selection 방법

# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) # Which variables were dropped/added?