##Linear Regression Boston dataset
library(MASS)
library(ISLR)
Boston
names(Boston)
head(Boston)
New data
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

attach(Boston)



fit1<-lm(Boston$medv~Boston$lstat, Boston)
fit1<-lm(medv~lstat, Boston)
fit1
summary(fit1)
plot(Boston$medv~Boston$lstat)

abline(fit1, lwd=1)
names(fit1)

coef(fit1)
resid(fit1)
confint(fit1)

predict(fit1, data.frame(lstat=c(25)), interval = "confidence")

predict(fit1, data.frame(lstat=c(20,21,25)), interval = "confidence")


describe(medv)


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


> plot(lstat, medv)
> abline(fit1)
> abline(fit1, lwd=2)
> plot(lstat, medv)
> abline(a=30, b=-0.7)
> abline(fit1, lwd=2, col="red")
> abline(fit1, lwd=2, col="red", pch=2)
> abline(fit1, lwd=2, col="red", pch=20)
> abline(fit1, lwd=2, col="red", pch="+")
> plot(lstat, medv)
> abline(fit1, lwd=2, col="red", pch="+")







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
             data
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
             
             
             #####Week 2 quiz Coursera R programming
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

#####Pollutant mean 1st function
pollutantmean <- function(directory, pollutant, id=1:332) {
  filelist <- list.files(path =directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id) {
    dt <- read.csv(filelist[i])
    values <- c(values, dt[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}
#####Test the 1st function
pollutantmean("~/Downloads/specdata", "sulfate", id=1:10)
pollutantmean("~/Downloads/specdata", "sulfate", id=1:110)
pollutantmean("~/Downloads/specdata", "sulfate", id=100:200)
pollutantmean("~/Downloads/specdata", "nitrate", id=100:200)


pollutantmean("~/Downloads/specdata", "sulfate", id=1:10)
pollutantmean("~/Downloads/specdata", "nitrate", id=70:72)
pollutantmean("~/Downloads/specdata", "sulfate", id=34)
pollutantmean("~/Downloads/specdata", "nitrate")


####
files <- list.files(path ="~/Downloads/specdata", pattern = ".csv", full.names = TRUE)
sum(complete.cases(read.csv(files[332])))

##### 2ND function

library(plyr)
complete <- function(directory, id=1:332) {
  files <- list.files(path =directory, pattern = ".csv", full.names = TRUE)
  nobs<-numeric()
  
  for (i in id) {
    
    data<-read.csv(files[i])
    nobs<-c(nobs, sum(complete.cases(data)))
}
result<-data.frame(id, nobs)
return(result)

}



##### Test 2nd function
complete("~/Downloads/specdata", 1)

complete("~/Downloads/specdata", 3)

complete("~/Downloads/specdata", c(2,4,8,10,12))
complete("~/Downloads/specdata", id=10:30)





print(cc$nobs)

cc<-complete("~/Downloads/specdata", 54)


##### 3rd function
dt<-dir("~/Downloads/specdata", full.names = T) %>% map_df(read.csv, na.rm=T)


correlation <- function(directory, threshold = 0) {
 
  files <- list.files(path =directory, pattern = ".csv", full.names = TRUE)
   
   data<-read.csv(files)
   nobs<-sum(complete.cases(data))
   data2<-complete.cases(read.csv(files))
   
   if (nobs > threshold) {
     for (i in data2) {
       cor<-cor(data2[[,2]], data2[[,3]], use = "complete.obs")
     }
     return(cor)
   }
}
  
######
####COMPLETE function

complete <- function(directory, id = 1:332) {
  # --- Assert 'directory' is a character vector of length 1 indicating the
  # location of the CSV files.  'id' is an integer vector indicating the
  # monitor ID numbers to be used Return a data frame of the form: id nobs 1
  # 117 2 1041 ...  where 'id' is the monitor ID number and 'nobs' is the
  # number of complete cases
  
  # --- Assert create an empty vector
  nobsNum <- numeric(0)
  
  for (cid in id) {
    # --- Assert get data frame as ID
    cDfr <- getmonitor(cid, directory)
    
    # --- Assert count the number of complete cases and append to numeric
    # vector
    nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
  }
  
  # --- Assert return value is a data frame with TWO (2) columns
  data.frame(id = id, nobs = nobsNum)
}

#####

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  tcorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
  return (tcorrs)
}




correlation("specdata")
    
cr<-corr("~/Downloads/specdata", threshold = 330)
cr

rm(list = ls())



cr<-corr("~/Downloads/specdata", threshold = 330)   
cr <- sort(cr)
cr <- rank(cr)
cr <- order(cr)



RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


  
  return(cor_vector)
  

    #####
corr <- function(directory, threshold = 0) {
  # --- Assert 'directory' is a character vector of length 1 indicating the
  # location of the CSV files.  'threshold' is a numeric vector of length 1
  # indicating the number of completely observed observations (on all
  # variables) required to compute the correlation between nitrate and
  # sulfate; the default is 0.  Return a numeric vector of correlations.
  
  # --- Assert create an empty numeric vector
  corrsNum <- numeric(0)
  
  # --- Assert get a data frame as ID = 1:332
  nobsDfr <- complete("specdata")
  
  # --- Assert apply threshold
  nobsDfr <- nobsDfr[nobsDfr$nobs > threshold, ]
  
  for (cid in nobsDfr$id) {
    # --- Assert get a data frame as ID in $id
    monDfr <- getmonitor(cid, directory)
    
    # --- Assert calculate correlation between $sulfate and $nitrate
    corrsNum <- c(corrsNum, cor(monDfr$sulfate, monDfr$nitrate, use = "pairwise.complete.obs"))
  }
  
  # --- Assert return value is a numeric vector of correlations
  return(corrsNum)
}

    
    
#####Test 3rd function

cr <- corr("~/Downloads/specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)










setwd("~/Downloads")
    

#### Test function
correlation("specdata", 0)

correlation("~/Downloads/specdata", 5000)


cr <- corr("~/Downloads/specdata", 2000)                
n <- length(cr)                
cr <- corr("~/Downloads/specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

######
dt
head(dt)
datbad<-is.na(dt)
datbad
dat<-dt[!datbad]
head(dat)
dim(dt)
dim(dat)









\cor<-cor(dat, dat[[,3]], method = c("pearson"))


corr<-function(dat, pollutant1, pollutant2) {
  dat<-complete.cases(dt)
  cor<-cor(dat[[,2]], dat[[,3]], method = c("pearson"))

}

#####practice week 3
x<-c(rnorm(10), runif(10), rnorm(10,1))
f<-gl(3,10)
x
f
tapply(x, f, mean)
tapply(x, f, mean)

x<-list(a=1:5, b=rnorm(10))
x
lapply(x, range)
lapply(x,mean)


sapply(x, range)
sapply(x,mean)

X<-array(2,3, dimnames=NULL)
array()