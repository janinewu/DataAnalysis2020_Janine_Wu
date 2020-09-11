#********************************************************************************
# Data Analysics Lab1
# Janine Wu
# 2020-09-011
#********************************************************************************

#Clear working space
rm(list = ls())
#Set working directory
setwd("D:/rpi/2020/Data Analytics/DataAnalysis2020_Janine_Wu/lab1")

GPW_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv", header = TRUE)
EPI_data <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1)

#View(EPI_data)
attach(EPI_data) 
#fix(EPI_data) 
EPI
tf <- is.na(EPI)
E <- EPI[!tf]

summary(EPI)
fivenum(EPI,na.rm=TRUE)
#help(stem)
stem(EPI)

#help(hist)
jpeg(file="hist_EPI.jpeg")
hist(EPI)
dev.off()
jpeg(file="histseq_EPI.jpeg")
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
#help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.))
#help(rug)
rug(EPI)
dev.off()


#Cumulative Density Function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 

#Quantile-Quantile
par(pty="s") 
qqnorm(EPI); qqline(EPI)

#Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)

#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)



#################################
#   Exercise 1
#################################

#DALY
tf <- is.na(DALY)
D <- DALY[!tf]

summary(DALY)
fivenum(DALY,na.rm=TRUE)
stem(DALY)

hist(DALY)
hist(DALY, seq(0., 100., 1.0), prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(DALY); qqline(DALY)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)


#WATER_H
tf <- is.na(WATER_H)
WH <- WATER_H[!tf]

summary(WATER_H)
fivenum(WATER_H,na.rm=TRUE)
stem(WATER_H)

hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(WATER_H); qqline(WATER_H)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

boxplot(EPI,WATER_H) 
qqplot(EPI,WATER_H)

boxplot(DALY,WATER_H)
qqplot(DALY,WATER_H)


#################################
#   Exercise 2
#################################

EPILand<-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand,na.rm=TRUE,bw=1.))
rug(ELand)

#No_surface_water
EPIWater <-EPI[!No_surface_water]
EWater <- EPIWater[!is.na(EPIWater)]
hist(EWater)
hist(EWater, seq(30., 95., 1.0), prob=TRUE)
lines(density(EWater,na.rm=TRUE,bw=1.))
rug(EWater)

#Desert
EPIDestert <-EPI[!Desert]
EDesert <- EPIDestert[!is.na(EPIDestert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)
lines(density(EDesert,na.rm=TRUE,bw=1.))
rug(EDesert)

#High_Population_Density
EPIPopulation <-EPI[!High_Population_Density]
EPopulation <- EPIPopulation[!is.na(EPIPopulation)]
hist(EPopulation)
hist(EPopulation, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPopulation,na.rm=TRUE,bw=1.))
rug(EPopulation)

#South_Asia
EPISAsia <- EPI[grepl('Asia',EPI_regions)]
ESAsia <- EPISAsia[!is.na(EPISAsia)]
hist(ESAsia)
hist(ESAsia, seq(35., 75., 1.0), prob=TRUE)
lines(density(ESAsia,na.rm=TRUE,bw=1.))
rug(ESAsia)
                