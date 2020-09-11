#********************************************************************************
# Data Analysics Lab1
# Janine Wu
# 2020-09-04
#********************************************************************************

#Clear working space
rm(list = ls())
#Set working directory
setwd("D:/rpi/2020/Data Analytics/lab1")


# Boston
#**********************************
#install.packages("MASS") 
library(MASS)
attach(Boston)
?Boston 
head(Boston)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston) 
summary(Boston$crim)

# ISLR
#**********************************
#install.packages("ISLR") 
library(ISLR)
data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))


#EPI_data
#***********************************
EPI_data <- read.csv("EPI_data.csv", header = TRUE)
names(EPI_data)
#missing values
is.na(EPI_data) 
colSums(is.na(EPI_data))
#explore distribution
summary(EPI_data$EPI)
boxplot(EPI_data$EPI)
fivenum(EPI_data$EPI,na.rm=TRUE)
hist(EPI_data$EPI)
