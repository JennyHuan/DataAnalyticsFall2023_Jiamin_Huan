# Group1 - Lab2
# kmeans 1
#fragment
set.seed(123)
sim.xy <- function(n, mean, sd) cbind(rnorm(n, mean[1], sd[1]),rnorm(n, mean[2],sd[2]))
# generate three clouds of points, well separated in the 2D plane
xy <- rbind(sim.xy(100, c(0,0), c(.2,.2)),sim.xy(100, c(2.5,0), c(.4,.2)),sim.xy(100, c(1.25,.5), c(.3,.2)))
xy[1,] <- c(0,2)     # convert 1st obs. to an outlying value
#
km3 <- kmeans(xy, 3) # ask for three clusters
plot(xy, col=km3$cluster)
cex=2.0
points(km3$centers, pch=3)
#
km4 <- kmeans(xy, 4) # ask for four clusters
cex=1.0
plot(xy, col=km4$cluster)
cex=2.0
points(km4$centers, pch=3)

# kmeans 2
data("iris")
iris.dist <- dist(iris[, -5])
iris.mds <- cmdscale(iris.dist)
# iris$Species is the 5th column
c.chars <- c("*", "o", "+")[as.integer(iris$Species)]
# iris$Species is the 5th column
# KMEANSRESULT is the variable you used in your kmeans lab assignment for the return variable.
a.cols <- rainbow(3)[KMEANSRESULT$cluster]

plot(iris.mds, col = a.cols, pch = c.chars, xlab = "X", ylab = "Y")
plot of chunk unnamed-chunk-5

# knn1
# read data in
nyt1<-read.csv(“nyt1.csv")
# eliminate zeros
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
## or could just have this: nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]
#90% to train
sampling.rate=0.9
#remainder to test
num.test.set.labels=nnyt1*(1.-sampling.rate)
#construct a random set of training indices (training)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
#build the training set (train)
train<-subset(nyt1[training,],select=c(Age,Impressions))
#construct the remaining test indices (testing)
testing<-setdiff(1:nnyt1,training)
#define the test set
test<-subset(nyt1[testing,],select=c(Age,Impressions))
#construct labels for another variable (Gender) in the training set
cg<-nyt1$Gender[training]
#construct true labels the other variable in the test set
true.labels<-nyt1$Gender[testing]
#run the classifier, can change k
classif<-knn(train,test,cg,k=5)
#view the classifier
classif
#looks at attriburtes
attributes(.Last.value)

# more in later classes

# knn2
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
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
	[(iris.valid$Species != fit)+1])