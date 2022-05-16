data(iris)
head(iris)

str(iris)

dim(iris)

# install.packages("caTools")
# install.packages("randomForest")
library(caTools)
library(randomForest)

split <- sample.split(iris,SplitRatio = 0.7)
split

train <- subset(iris,split == "TRUE")
test <- subset(iris,split == "FALSE")

dim(train)
dim(test)

# Fitting the RF to train dataset
set.seed(120)

classifier_RF = randomForest(x=train[-5],y=train$Species,ntree=500)
classifier_RF

# Predicting the test set results
y_pred = predict(classifier_RF,newdata = test[-5])

# confusion matrix

conf_mat = table(test[,5],y_pred)
conf_mat

# Plot model
plot(classifier_RF)

# Feature importance
importance(classifier_RF)

#variable imp plot
varImpPlot(classifier_RF)
