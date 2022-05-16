##Example
## The in-built data set "mtcars" describes different models of a car with their various engine specifications.
##In "mtcars" data set, the transmission mode (automatic or manual) is described by 
##the column am which is a binary value (0 or 1). We can create a logistic regression model 
##between the columns "am" and 3 other columns - hp, wt and cyl.

data <- mtcars
data
head(data)
sum(is.na(data))
summary(data)

xtabs(~ am+cyl, data = data)
table(data$am, data$cyl)
data$cyl <- as.factor(data$cyl)
logit2 <- glm(am~hp+wt+cyl, data = data, family = "binomial")
logit2
summary(logit2)

y <- data.frame(hp=110, wt=2.598, cyl=as.factor(6))
pre <- predict(logit2,y)
pre

y <- data.frame(hp=160, wt=1.85, cyl=as.factor(6))
pre <- predict(logit2,y)
pre
