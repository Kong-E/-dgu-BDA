df <- data.frame( y=c(800,840,820,860,900,880,920),
                  x1=c(20,23,20,25,17,29,30),
                  x2=c(10,15,18,19,20,26,25),
                  x3=c(30,27,27,24,21,21,18))
fit<-lm(y~., data = df)
step <- step(fit, direction = "backward")
summary(step)

options(scipen = 999)
df2 <- data.frame( y=c(2,0,0),
                  x=c(1,1,0))
lm(y~.,data = df2)
