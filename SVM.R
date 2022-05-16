# Importing packages
dataset = read.csv('Social_Network_Ads.csv')
head(dataset)
dim(dataset)
dataset = dataset[3:5]
dataset

# Encoding
dataset$Purchased = factor(dataset$Purchased,level=c(0,1))
class(dataset$Purchased)

# Splitting data
## install.packages("caTools")
library(caTools)

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
split
training_set = subset(dataset,split == TRUE)
dim(training_set)
test_set  = subset(dataset,split == FALSE)
dim(test_set)

# Feature scaling
training_set[-3]=scale(training_set[-3])
test_set[-3]=scale(test_set[-3])

# Fitting SVM to the training set
library(e1071)

classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the test set results
y_pred = predict(classifier,newdata=test_set[-3])

# Making confusion matrix
cm = table(test_set[,3],y_pred)
cm

# Visualizing the training set
# Download package tarball from CRAN archive

url <- "https://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.2.tar.gz"
pkgFile <- "ElemStatLearn_2015.6.26.2.tar.gz"
download.file(url = url, destfile = pkgFile)
# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)
# Delete package tarball
unlink(pkgFile)
library(ElemStatLearn)


# Plotting the training data set results
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
#expand.grid() - Create a data frame from all combinations of the supplied vectors or factors.
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

#Create a contour plot, or add contour lines to an existing plot
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine'))
#points is a generic function to draw a sequence of points at the specified coordinates. The specified character(s) are plotted, centered at the coordinates.

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# Visualiizing test set
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Since in the result, a hyper-plane has been found in the Training set result and verified to be the best one in the Test set result. Hence, SVM has been successfully implemented in R. 
