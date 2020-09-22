#********************************************************************************
# Data Analysics Lab1-part2
# Janine Wu
# 2020-09-22
#********************************************************************************

#Clear working space
rm(list = ls())
#Set working directory
setwd("D:/rpi/2020/Data Analytics/DataAnalysis2020_Janine_Wu/lab1")

EPI_data <- read.csv("EPI_data.csv", header = TRUE)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")

qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot

x <- seq(30,95,1)
x2 <-seq(30,95,2)
x2 <-seq(30,96,2)
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)


plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")

#Explore DALY
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE)
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)

#Explore WATER_H
plot(ecdf(EPI_data$WATER_H),do.points=TRUE,verticals = TRUE)
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)

#Inter-compare
qqplot(EPI_data$EPI, EPI_data$DALY)
qqplot(EPI_data$EPI, EPI_data$WATER_H)

boxplot(EPI_data$EPI, EPI_data$ENVHEALTH, EPI_data$ECOSYSTEM,
        EPI_data$DALY, EPI_data$AIR_H, EPI_data$WATER_H,
        EPI_data$AIR_E, EPI_data$WATER_E, EPI_data$BIODIVERSITY,
        names = c("EPI","ENVHEALTH", "ECOSYS", "DALY", 
        "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIV"),
        las=2,
        cex.axis=0.8)



multivariate <-read.csv("multivariate.csv", header = TRUE)
head(multivariate)
attach(multivariate)
mm <-lm(Homeowners ~ Immigrant)
summary(mm)

plot(Homeowners~Immigrant)
abline(mm)
abline(mm, col=2, lwd=3)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients


