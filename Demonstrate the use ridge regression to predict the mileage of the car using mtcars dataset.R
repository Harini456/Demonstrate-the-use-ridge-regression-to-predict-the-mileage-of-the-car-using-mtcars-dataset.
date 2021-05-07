#Name: Harini G
#Roll No: 2048034
#Lab6

#1.Load mtcars dataset
data(mtcars)
#?mtcars

#2.install ridge and glmnet packages
#install.packages("glmnet")
#install.packages("ridge")

#3.Perform the exploratory data analysis
nrow(mtcars)
ncol(mtcars)
#inference:the dataset contains 32 rows and 11 coulmns
head(mtcars)
tail(mtcars)
summary(mtcars)

#Historgram
hist(mtcars$mpg)
#inference: miles/ gallon is left skewed
hist(mtcars$disp)
#inference: the displacement is having Multi-Modal Distribution
hist(mtcars$hp)
#inference: gross horsepower is left skewed
hist(mtcars$drat)
#inference: Rear axle ratio is following normal distribution
hist(mtcars$wt)
#inference: weight is having The Bi-Modal Distribution
hist(mtcars$qsec)
#inference: mile time is following normal distribution
hist(mtcars$carb)
#inference: carburetors is having The Bi-Modal Distribution

#boxplot
boxplot(mtcars$mpg, main="mpg")
boxplot(mtcars$disp, main="disp")
boxplot(mtcars$hp, main="hp")
boxplot(mtcars$drat, main="drat")
boxplot(mtcars$wt, main="wt")
boxplot(mtcars$qsec, main="qsec")


#4.Choose optimum lamba value
x_var=model.matrix(mpg~.,mtcars)[,-1]
y_var=mtcars$mpg

lambda_seq =10^seq(2, -2, by = -.1)
set.seed(86)
train = sort(sample(1:nrow(x_var),0.8*nrow(x_var)))
x_test = (-train)
y_test = y_var[x_test]


#5.Extract the model using k-cross validation
library(glmnet)
cv_output = cv.glmnet(x_var[train,], y_var[train], alpha = 1, lambda = lambda_seq)
best_lambda= cv_output$lambda.min
best_lambda

best_fit=cv_output$glmnet.fit
summary(best_fit)


#6.Build the final model and interpret
library(ridge)
inputData = data.frame (mtcars)
trainingData=inputData[train, ]
testData = inputData[-train, ]
linRidgeMod =linearRidge(y_var ~ x_var, data = trainingData)
linRidgeMod
predicted=predict(linRidgeMod, testData) # predict on test data
compare = cbind (actual=testData$response, predicted) # combine
compare
mean (apply(compare, 1, min)/apply(compare, 1, max))
#accuracy is 1