# Lab3
# Heatmap(), image() and hierarchical clustering example
# creating a matrix data with random numbers
# and plotting the matrix using the image() function
# you will see there, it does not have a real pattern in the plot.
set.seed(12345)
help(par)
# par can be used to set or query graphical parameters.
# Parameters can be set by specifying them as arguments
# to par in tag = value form, or by passing them as a list of tagged values.
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
help("heatmap")
help(rep)
par(mar=rep(0.2,4))
heatmap(data_Matrix)
# When we run the heatmap() here, we get the dendrograms printed on the
# both columns and the rows and still there is no real immerging pattern that is
# interesting to us,
# it is because there is no real interesting pattern underlying in the data we generated.
help("rbinom") 
set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}
# during the coin flip, if is it turn out to be one
# (true), then, just added a pattern to my data in a
# way that the five of the columns have a mean
# of zero and others have mean of three.
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])
par(mar=rep(0.2, 4))
heatmap(data_Matrix)
hh <- hclust(dist(data_Matrix))
data_Matrix_ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_ordered)[, nrow(data_Matrix_ordered):1])
plot(rowMeans(data_Matrix_ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch =19)
plot(rowMeans(data_Matrix_ordered), xlab = "Column", ylab = "Column Mean", pch =19)

# ctree2
require(party)
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)

# kknn1
require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")[(iris.valid$Species != fit)+1])

# kknn2
require(kknn)
data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

# kknn3
data(swiss)
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20,
      main = "Swiss data, Education < 20")

# kmeans1
data(swiss)
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])    

# nyt1
nyt1<-read.csv("nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]		# shrink it down!
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
library(class)
classif<-knn(train,test,cg,k=5) #
classif
attributes(.Last.value) 

data("Titanic")
library(rpart)
tree_model <- rpart(Survived ~ ., data = Titanic)
plot(tree_model)
text(tree_model, pretty = 0)
library(party)
ctree_model <- ctree(Survived ~ ., data = Titanic)
plot(ctree_model)
titanic_df <- as.data.frame(Titanic)
cluster_data <- titanic_df[, -c(1, 2)]
hclust_result <- hclust(dist(cluster_data))
plot(hclust_result)
