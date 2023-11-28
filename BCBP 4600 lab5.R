mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt, data = mtcars)
model1
plot(model1, pch = 18, col = "red", which = c(4))
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)
CooksDistance
round(CooksDistance, 5)
sort(round(CooksDistance, 5))

library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData <- na.omit(Hitters)
dim(HittersData)
library(tidyverse)
glimpse(HittersData)
head(HittersData)
SalaryPredictModel1 <- lm(Salary ~ ., data = HittersData)
summary(SalaryPredictModel1)

cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

# Model 2
SalaryPredictModel2 <- lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)

# Normality Tests
help("rnorm")
set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)
help("shapiro.test")
shapiro.test(data1)
hist(data1, col = "green")
shapiro.test(data2)
hist(data2, col = "steelblue")

set.seed(0)
data <- rnorm(100)
shapiro.test(data)

help("rpois")
data <- rpois(n=100, lambda = 3)
shapiro.test(data)
hist(data, col = "yellow")

library(nortest)
set.seed(1)
x <- rnorm(100, 0, 1)
ad.test(x)

set.seed(1)
x <- runif(100, 0, 1)
ad.test(x)

# lab3_ctree1
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
#find "prettier" ways to plot the tree

# lab3_ctree2
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")

# lab3_ctree3
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")

# lab3_kknn1
require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,] 	# train
iris.valid <- iris[val,]	# test
iris.kknn <- train.kknn(Species~., iris.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(iris.kknn)
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$W)
head(iris.kknn$D)
head(iris.kknn$C)
head(iris.kknn$fitted.values)

# lab3_randomforest1
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)

# perform randomForest and other tree methods.....
