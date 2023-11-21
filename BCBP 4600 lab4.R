# KNN example using ISLR package (Textbook) 
library(ISLR) # you need to install the ISLR package first
# Caravan dataset is about the insurance
library(class)
head(Caravan)
str(Caravan)
# Purchase: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# "Yes" or "No" indicates wheater people purchased the insurance policy or not. 
dim(Caravan) # it has 5822 rows (observations) and 86 features/columns  
summary(Caravan)
summary(Caravan$Purchase)
# Yes: 348 people purchased the insurance and No:5474 people did not purchase the insurance
# check for any NA and missing values
any(is.na(Caravan)) # FALSE, that means no NA values in this dataset 
# Now we want to check the scales of the variables becuase the variable with a large scale
# will have a larger effect on distances between observations when using the KNN Algorithm 
# Checking the Variances on features/Colums using the var() function in R
var(Caravan[,1]) # Variance of the 1st column is 165.0378
var(Caravan[,2]) # Variance of the 2nd column is 0.1647
var(Caravan[,3]) # Variance of the 3rd column is 0.6238
# You can see that the variances are different for each of the column variables, 1st one is 165.03 and 2nd one is 0.16
# and there is a huge difference, because of that we want to standardize the variables. 
# We will do that for all the columns except the "Purchase" variable which we are going predict.
purchase <- Caravan[,86] # you can write the same as 
# purchase <-Caravan[,'Purchase'] with the column name, we use the column number 86 for the simplcity.
purchase
# Now we want to Standardize the columns except the 86th column
StandardizedCaravan <- scale(Caravan[,-86]) # when we use -86 it will not include the 86th column.
var(StandardizedCaravan[,1])
var(StandardizedCaravan[,2])
var(StandardizedCaravan[,3])

# test set
test_index <- 1:1000
test_data <- StandardizedCaravan[test_index,]
test_purchase <- purchase[test_index]

# train set
train_data <- StandardizedCaravan[-test_index,]
train_purchase <- purchase[-test_index]

# set seed
set.seed(101)
predicted_purchase <- knn(train_data,test_data,train_purchase, k = 10)
head(predicted_purchase)

missClassError <- mean(test_purchase != predicted_purchase)
print(missClassError)

# Choosing the K value
# we can write a for-loop
predicted_purchase <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k =i)
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)

# Plot the K value on a graph
library(ggplot2)
k_values <- 1:20

error_df <- data.frame(error_rate, k_values)
print(error_df)
ggplot(error_df,aes(k_values,error_rate)) + geom_point() + geom_line(lty='dotted', color='blue')

# ISLR Package (ISRL 7th Eidtion Textbook )
# KMeans example with Iris dataset
# KMeans is an Unsupervised technique 
library(ISLR)
head(iris)
str(iris)
# visually inspecting the three factors: Species clusters with a plot
plot1 <- ggplot(iris, aes(Petal.Length,Petal.Width, color=Species))
print(plot1 + geom_point(size=3))

# set seed for the random numbers
set.seed(101)
# Read the documentation for KMeans in R-Studio
help("kmeans")

irisClusters <- kmeans(iris[,1:4], 3, nstart = 20) # nstart is the number of random start
print(irisClusters)
table(irisClusters$cluster,iris$Species)
# plotting the clusters
library(cluster)
help("clusplot")
clusplot(iris,irisClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)

# Outlier Example
data("cars")
cars1 <- cars[1:30,]
head(cars1)
cars_outliers <- data.frame(speed = c(19,19,20,20,20), dist = c(190,186,210,220,218))
head(cars_outliers)
cars2 <- rbind(cars1, cars_outliers)
help(par)
par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, xlim = c(0,28), ylim = c(0,230), 
     main = "With Outliers", xlab = "Speed", ylab = "Dist", pch = "*", 
     col = "red", cex = 2)
abline(lm(dist~speed, data = cars2), col = "blue", lwd = 3, lty = 2)
plot(cars1$speed, cars1$dist, xlim = c(0,28), ylim = c(0,230), 
     main = "Outliers Removed", xlab = "Speed", ylab = "Dist", pch = "*", 
     col = "red", cex = 2)
abline(lm(dist~speed, data = cars1), col = "blue", lwd = 3, lty = 2)

# lab2_abalone
# read data in
library(tidyverse)
aba<-read.csv("abalone.csv") %>% na.omit()
naba<-dim(aba)[1]
#90% to train
sampling.rate=0.9
#remainder to test
num.test.set.labels=naba*(1.-sampling.rate)
#construct a random set of training indices (training)
training <-sample(1:naba,sampling.rate*naba, replace=FALSE)
#build the training set (train)
train<-subset(aba[training,],select=c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight"))
#construct the remaining test indices (testing)
testing<-setdiff(1:naba,training)
#define the test set
test<-subset(aba[testing,],select=c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight"))
#construct labels for another variable (Rings) in the training set
crings<-aba$Rings[training]
#construct true labels the other variable in the test set
true.labels<-aba$Rings[testing]
#run the classifier, can change k
classif<-knn(train,test,crings,k=5)
#view the classifier
classif
#looks at attributes
attributes(.Last.value)

#now do bayes.

# lab2_nbayes1
require(mlbench)
library(e1071)
data(HouseVotes84)
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")

pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)

## Example of using a contingency table:
data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic)[,1:3])

## Example with metric predictors:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])

# lab2_nbayes2
data(Titanic)
mdl <- naiveBayes(Survived ~ ., data = Titanic)
mdl
# etc.

# lab2_nbayes3
require(mlbench)
data(HouseVotes84)
library(klaR)
model <- NaiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])

pred <- predict(model, HouseVotes84[,-1])
table(pred$class, HouseVotes84$Class)


# lab2_swiss
data(swiss)
sclass <- kmeans(swiss[2:6],3) 
table(sclass$cluster, swiss[,2])    
# 
library(e1071)
m <- naiveBayes(swiss[2:6], swiss[,2])    
table(predict(m, swiss[, 2:6], type = "class"), swiss[, 2])

      