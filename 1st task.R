df=read.table("Book1.csv",fileEncoding = "UTF-8-BOM",sep=",",header=TRUE)
library(dplyr)
library(ggplot2)
library(rpart)
library(caret)

hours=df$Hours
scores=df$Scores
#we ploted to see if there is any linear relation between the percentage of scores and hours of study
plot(x=hours,y=scores)
# we found that there is a +ve linear relationship
set.seed(12345)

#partitioning data into train and test data

train_rows <- createDataPartition(scores, p = 0.2,list=FALSE)
train <- df[train_rows,]
test <- df[-train_rows,]

summary(train)
summary(test)
# There is no outliers or na values in the data
# now we need to fit our train data in a linear regression

reg=lm(Scores~.,data=train)
summary(reg)

#plotting the regression line and the test points
plot(hours,scores,col="red")
abline(reg)


#predicting the values of scores
my_test_predict <- predict(reg,test)
test1<-as.numeric(my_test_predict)
new_test<-data.frame(test,test1)
new_test

# predicting the percentage score of student based on a 7 hour study
hrs<-data.frame(Hours=7)
my_pred<-predict(reg,hrs)
my_pred

#evaluating the model
# Using the adjusted  R^2 we can say
#how well the actual outcomes are replicated by the regression line, 
#here R^2 is 0.96, so this model is a good model.

