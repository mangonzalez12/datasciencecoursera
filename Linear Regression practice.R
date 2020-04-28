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

extract.data<-function(chas, indus){
  new.data<-cbind(chas, indus)
  return(new.data)
}
x<-extract.data(chas, indus)
x
head(x)

Boston



fit1<-lm(Boston$medv~Boston$lstat, Boston)
fit1
summary(fit1)
plot(Boston$medv~Boston$lstat)

abline(fit1, lwd=3)

fit2<-lm(Boston$medv~Boston$lstat, Boston$chas, Boston)



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
             
             
#####Practice IF, loop, while loop
             
             data<-cbind(x,y)
             x
             y<-c(4,5,6,7,8)
             data<-cbind(x,y)
             data
             data[[1]]
             data[1,]
             
             data[,1]
             class(data)
             as.data.frame(data)
             subset.data<-if(as.data.frame(data)[,1]>2){
               print(x)
             }
             is.na(x)
             bad<-is.na(x)
             x[!bad]
             
             for (i in data[,1]) {
               i=i+2
               print(i)
             }
             
             number<-0
             while(data[,1])==2{
               data[,1]==
                 print(number)
             }
             
             for (a in data[,1]) {
               a<-cor(data[,1], data[,2])
               print(a)
             }
             
             plot(data[,1], data[,2])
             
             
             
             lm<-function(x){
               puta<-x*2
               print(puta)
             }
             
             
             #####Week 2 quiz
             cube<-function(x,n){
               x^3
             }
             cube(3)
             
             x<-1:10
             if(x>5){
               x<-0
             }
             
             
             f<-function(x) {
               g<-function(y){
                 y+z
               }
               z<-4
               x +g(x)
             }
             
             x<-5
             y<-if(x<3) {
               NA
             } else {
               10
             }
        
            ###Assignment CSV files
             setwd("~/Downloads")
             data<-list.files(pattern = "*.csv")
             myfiles<-lapply(as.data.frame(data), read.delim)
             dim(myfiles)
             head(myfiles)
             myfiles
rm(list = ls())
             
library(plyr)
install.packages("readr")
library(readr)

setwd("~/Downloads/specdata")
mypath<-"specdata"
file_names<-list.files(path = mypath, pattern = "*.csv", full.names=TRUE)
data<-lapply(file_names, read.csv)
names(data)<-c("Date", "Sulfate", "Nitrate", "id")

setwd("~/Downloads/specdata")
mypath<-"specdata"
file_names<-list.files(path = mypath, pattern = "*.csv", full.names=TRUE)
data<-lapply(file_names, read.csv)
data2 <- do.call(cbind, data)
names(data)<-c("Date", "Sulfate", "Nitrate", "id")




######Assignment Coursera 2nd Week
install.packages("tidyverse")
library(tidyverse)
####Load tidyverse or purrr and readr
dt<-dir("~/Downloads/specdata", full.names = T) %>% map_df(read.csv)

head(dt)

Part 1
Write a function named 'pollutantmean' that calculates the mean of 
a pollutant (sulfate or nitrate) across a specified list of monitors. 
The function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads 
that monitors' particulate matter data from the directory specified 
in the 'directory' argument and returns the mean of the pollutant 
across all of the monitors, ignoring any missing values coded as NA.
A prototype of the function is as follows

pollutantmean<-function(directory, pollutant, id=1=332){
directory<-"~/Downloads/specdata"
}


        
             