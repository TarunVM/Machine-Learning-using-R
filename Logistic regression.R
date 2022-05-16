df<- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df
head(df)

sum(is.na(df))
summary(df)

xtabs(~admit +rank,data = df)

table(df$admit,df$rank)


df$rank = as.factor(df$rank)
# sometimes factor is also considered as data structure
# when you have to see numerical data into categorical view we use factor

logit=glm(admit~gre+gpa+rank,data = df, family = "binomial")
summary(logit)

x <- data.frame(gre=790,gpa=3.8,rank=as.factor(1))
p <- predict(logit,x)
p

x <- data.frame(gre=590,gpa=2.8,rank=as.factor(1))
p <- predict(logit,x)
p

