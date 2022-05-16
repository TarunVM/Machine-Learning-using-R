iris
mydf<-iris
mydf
head(mydf)

# generate 90% of random number from total number of rows
ran<- sample(1:nrow(iris),0.9*nrow(iris))
ran

#normalization function
nor<-function(x){(x-min(x))/(max(x)-min(x))}

#predictors

iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)],nor))
iris_norm

iris_train <- iris_norm[ran,]
iris_train
dim(iris_train)

iris_test <- iris_norm[-ran,]
iris_test
dim(iris_test)
iris_target_category <- iris[ran,5]
iris_target_category

iris_test_category <- iris[-ran,5]
iris_test_category

library(class)

pr <- knn(iris_train,iris_test,cl = iris_target_category,k = 13)

tab <- table(pr,iris_test_category)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)
