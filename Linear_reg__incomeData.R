library(ggpubr)
library(ggplot2)
library(broom)
library(dplyr)
library(ROCR)

setwd("C:/Users/Tarun/Desktop/DS/R/Supervised learning/Linear Regression")

income.data <- read.csv("income.data.csv")

head(income.data)

dim(income.data)
summary(income.data)

hist(income.data$happiness)

#Linearity
plot(happiness ~ income,data=income.data)

income.happiness.lm <- lm(happiness~income,data=income.data)
summary(income.happiness.lm)

par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))

income.graph <- ggplot(income.data, aes(x=income, y=happiness)) + geom_point()
income.graph

income.graph<- income.graph+geom_smooth(method = "lm",col="red")
income.graph

income.graph <- income.graph + stat_regline_equation(label.x=3,label.y = 7)
income.graph

#making graph ready
income.graph <- income.graph + theme_bw()+
  labs(title = "Reported Happiness as a function of Income",x = "Income(X$10000)",y="Happiness(1-10")
income.graph

#============ Example 2 ==================

?cars
head(cars)

plot(cars , col="blue", pch=20, cex=2, main="Relationship between speed and
            stopping distance for 50 cars", xlab="speed in mph", ylab="stopping distance in feet")
plot(cars)

sample(3)
set.seed(0)
sample(3)
sample(3)
set.seed(1)
sample(3)
set.seed(1)
sample(3)

#example for scale
mt <- matrix(1:10, ncol=5)
mt
scale(mt, center = TRUE, scale = FALSE)
#

speed.c <- scale(cars$speed,center = TRUE,scale=FALSE)
mod1 = lm(formula = dist~speed.c,data=cars)
mod1
summary(mod1)

#================= 3 -===================

heart <- read.csv("heart.data.csv")
head(heart)

dim(heart)
summary(heart)

cor(heart$biking,heart$smoking)
# not an issue correlation is very low

plot(heart.disease~biking,data=heart)

plot(heart.disease~smoking,data=heart)

#multilinear

heart.lm <- lm(heart.disease~biking+smoking, data=heart)
summary(heart.lm)
