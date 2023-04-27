housing.df <- read.csv("WestRoxbury.csv", header = TRUE); attach(housing.df)
dim(housing.df)
head(housing.df)
View(housing.df)

set.seed(1)

train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(housing.df),train.rows),
                     dim(housing.df)[1]*0.3)
test.rows <- setdiff(rownames(housing.df),union(train.rows, valid.rows))

# create the 3 data frames by collectiong all columns from the appropriate rows
train.data <- housing.df[train.rows,]
valid.data <- housing.df[valid.rows,]
test.data <- housing.df[test.rows,]

# 모델의 구축 및 평가 예시
reg <- lm(TOTAL.VALUE~.-TAX,
          data=housing.df,
          subset=train.rows)
tr.res <- data.frame(train.data$TOTAL.VALUE, 
                     reg$fitted.values,
                     reg$residuals)
head(tr.res)

# 검증데이터(validation data)를 사용하여 모델 검증
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE,
                     pred,
                     residuals=valid.data$TOTAL.VALUE - pred)
head(vl.res)

#모델의 평가측도(RMSE)를 계산
library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL.VALUE)
# compute accuracy on prediction set
accuracy(pred, valid.data$TOTAL.VALUE)
