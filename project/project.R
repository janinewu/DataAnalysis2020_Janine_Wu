#Clear working space
rm(list = ls())
#Set working directory
setwd("D:/rpi/2020/Data Analytics/DataAnalysis2020_Janine_Wu/project")

library(kknn)
library(rpart)
library(party)
library(ggplot2)
library(tidyr)
library(resample)


#Import dataset
data <- read.csv("lgbt_city_score.csv", header = TRUE)
summary(data)

#check for na
any(is.na(data))

#Clean data
score <- data[4:9]

attach(score)
#Explore
ggplot(gather(score, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 0.5, color="black", fill="#76D7C4", alpha = 0.5)+
  facet_wrap(~cols, scales = 'free_x')+
  theme_bw()

#hist
ggplot(gather(score, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 0.5, color="black", fill="#76D7C4", alpha = 0.5)+
  facet_wrap(~cols, scales = 'free_x')+
  theme_bw()

########################################################

#boxplot
ggplot(stack(score), aes(x = ind, y = values, fill=ind)) +
  geom_boxplot(alpha=0.4)+
  xlab("") + ylab("Score")+labs(fill="")+
#  scale_x_discrete(labels=c("dating" = "", "lgbt.nightlife" = "",
#                            "openness.in.the.city" = "",
#                            "safety" = "",
#                            "lgbt.rights" = "",
#                            "total" =""))+
  theme_classic()

summary(score)


########################################################
#Modeling

#linear
plot(openness.in.the.city, lgbt.rights+safety)
lm_op_rs <- lm(lgbt.rights+safety ~ openness.in.the.city)
summary(lm_op_rs)
abline(lm_op_rs, col="#76D7C4", lwd=2)


plot(lgbt.nightlife, total)
lm_nightlife <- lm(total ~ lgbt.nightlife)
summary(lm_nightlife)
abline(lm_nightlife, col="#76D7C4", lwd=2)


lm_total <- lm(total ~ dating+lgbt.nightlife+openness.in.the.city+safety+lgbt.rights)
summary(lm_total)


#clustering
m <- dim(score)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
score.learn <- score[-val,]
score.valid <- score[val,]
score.kknn <- kknn(total~., score.learn, score.valid, distance = 1,
                 kernel = "triangular")
summary(score.kknn)
fit <- as.integer(fitted(score.kknn))
table(as.integer(score.valid$total), fit)
pcol <- as.character(as.numeric(score$total))
pairs(score.valid[1:6], pch = pcol, col = c("gray","blue")[(score.valid$total != fit)+1])

#tree
tr <- ctree(total ~ ., data=score)
tr
plot(tr, type = "simple")

detach(score)

