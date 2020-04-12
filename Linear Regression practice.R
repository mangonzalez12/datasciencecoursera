##Linear Regression Boston dataset
library(MASS)
library(ISLR)
Boston
names(Boston)
head(Boston)

hist(Boston$crim)
hist(Boston$zn)
hist(Boston$indus)
hist(Boston$chas)
hist(Boston$nox)
hist(Boston$rm)

fit1<-lm(Boston$medv~Boston$lstat, Boston)
fit1
summary(fit1)
plot(Boston$medv~Boston$lstat)

cor(Boston$medv,Boston$lstat)
cor.test(Boston$medv,Boston$lstat)

attach(Boston)
fit1$coefficients
fit1$residuals

plot(medv~lstat)

hist(fit1$residuals)
fit1$effects
fit1$rank
fit1$fitted.values
fit1$assign
fit1$qr
fit1$df.residual


####Prediction, remember that prediction treats the function as a black box

predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "prediction")

####Plots

plot(lstat~medv)
abline(fit1)       

abline(fit1, lwd=3)
abline(fit1, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

######Make various graphs appear in the same plot screen
par(mfrow=c(2,2))
plot(fit1)

plot(predict(fit1, residuals(fit1)
plot(predict(fit1), rstudent(fit1))
plot(hatvalues(fit1))

plot(Boston$crim)
install.packages("psych")
describe(Boston)

plot(Boston$crim)
install.packages("psych")
describe(Boston)
