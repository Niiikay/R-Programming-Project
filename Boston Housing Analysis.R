#retrieving the dataset
library(MASS)


# Looking up the help file to understand more about the variables recorded
?Boston


# Exploring all predictors - qualitative/quantitative
str(Boston)


# Exploring range of the following predictors - crim, age, lstat
range(Boston$crim)

range(Boston$age)

range(Boston$lstat)


install.packages('ggplot2') 
require(ggplot2) 


# Returns the number of rows and columns of the dataset
dim(Boston)


# Sample partitioning and validation
sampleSize <- floor(0.70 * nrow(Boston))
set.seed(123)

train_ind <- sample(seq_len(nrow(Boston)), size = sampleSize)


# partitioning sample indices into the training set and the testing set
train <- Boston[train_ind, ]
test <- Boston[-train_ind, ]
dim(train)


t.test(train$medv,test$medv)
## If the p value of this test is more, it shows that medv is not a statistically significant term


# fitting a linear regression of medv on lstat.
lm.fit <- lm(medv ~ lstat, data = Boston)


#f regression results
summary(lm.fit)


#overlaying a linear regression line on a scatterplot of mdev vs. lstat
qplot(data = Boston, x = lstat, y = medv,
      xlab = "percent of lower status in the population", 
      ylab = "Median value of owner-occupied homes in per $1000s") +
  stat_smooth(method = "lm")


# scatterplot of mdev vs. age
qplot(data = Boston, x = medv, y = age, 
      xlab = "Median Value of owner occupied homes per $1000.", 
      ylab = "Proportion of owner-occupied units built prior to 1940") + 
  stat_smooth(method = "lm")


# fitting a linear regression of medv on lstat and age.
lm.fit.age = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit.age)


# fitting a multiple regression model to predict medv using all of the 13 predictors in Boston Housing Dataset
mult.fit.lm = lm(formula = medv ~., data = Boston)
summary(mult.fit.lm)


# Computing the mean squared errors for the all of the linear models fitted for the Boston Housing data set
lm.fit <- lm(medv ~ lstat, data = Boston)
mse <- with(Boston, mean((medv - predict(lm.fit, Boston))[-train_ind]^2))
mse

lm.fit.age <- lm(medv ~ lstat + age, data = Boston)
lm.fit.age
mse_age <- with(Boston, mean((medv - predict(lm.fit.age, Boston))[-train_ind]^2))
mse_age

mult.fit.lm = lm(formula = medv ~., data = Boston)
mse_lm <- with(Boston, mean((medv - predict(mult.fit.lm, Boston))[-train_ind]^2))
mse_lm