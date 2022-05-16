data = read.csv("german_credit.csv")
head(data)

names(data)
str(data)
dim(data)

typeof(data$Creditability)
class(data$Creditability)


#make dependent a catagorical data type
data$Creditability = as.factor(data$Creditability)

typeof(data$Creditability)
class(data$Creditability)

set.seed(123)
#split data of 70%
dt=sample(nrow(data),nrow(data)*0.7)
dt
#sorting the data
dt=sort(dt)
dt

train=data[dt,]
val=data[-dt,]

#checking the nrows after splitting
nrow(train)
nrow(val)

#to view dataset
edit(val)

# DECISION TREE MODEL
#add package rpart
library(rpart)
mtree <- rpart(Creditability~.,data=train,method="class",
               control=rpart.control(minsplit = 20,
                                     minbucket = 7,
                                     maxdepth = 10,
                                     usesurrogate = 2,
                                     xval = 10))

mtree
plot(mtree)
text(mtree)

library(RColorBrewer)
library(rattle)
library(rpart.plot)

# view 1
prp(mtree,faclen =0,cex = 0.8,extra = 1)

# for better view
prp(mtree,faclen =1,cex = 0.7,extra = 2)

# View 2
tot_count = function(x,labs,digits,varlen)
{paste(labs,"\n\nn=x$frame$n")}
prp(mtree,faclen=0,cex=0.8,node.fun = tot_count)

# Pruning

printcp(mtree)

# To access cp table
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]
bestcp

# Prune the tree using bestcp
pruned <- prune(mtree,cp=bestcp)
prp(pruned,faclen = 0,cex = 0.8,extra = 1)

# Confusion matrix
conf.matrix <- table(train$Creditability,predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual",rownames(conf.matrix),sep=":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#test set
conf.matrix <- table(val$Creditability,predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual",rownames(conf.matrix),sep=":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


accuracy_test <- sum(diag(conf.matrix))/sum(conf.matrix)
accuracy_test

