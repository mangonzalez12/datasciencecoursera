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
attach(Boston)
