# Lab1 - Part1
EPI_data <- read.csv(file.choose(), skip = 1)
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
attach(EPI_data) 	
# sets the ‘default’ object
fix(EPI_data) 	
# launches a simple data editor
?fix
EPI			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array
E
summary(EPI)
fivenum(EPI)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), probability = TRUE)
lines(density(EPI, na.rm = TRUE, bw = 1))
lines(density(EPI, na.rm = TRUE, bw = "SJ"))
help(rug)
rug(EPI)
help(stem)

# Exercise 1 - fitting a distribution
# Cumulative Density Function
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
# Quantile to Quantile
par(pty = "s")
qqnorm(EPI)
qqline(EPI)
# Simulated data from t-distribution:
x1 <- rt(250, df = 5)
qqnorm(x1); qqline(x1)
qqplot(qt(ppoints(250), df = 5), x1, xlab = "Q-Q plot for t dsn")
qqline(x1)
# Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

# practice 1 - fitting a distribution
DALY
t <- is.na(DALY)
D <- DALY[!t]
D
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(DALY)
qqline(DALY)
AIR_H
f <- is.na(AIR_H)
A <- AIR_H[!f]
A
plot(ecdf(AIR_H), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(AIR_H)
qqline(AIR_H)

# Comparing Distributions
boxplot(EPI, DALY)
# Self-Practice
boxplot(DALY, AIR_H)

# Distribution
help("distribution")

# Exercise 2: Filtering (populations)
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30, 95, 1.0), probability = TRUE)

# Practice 2
EPIRegion <- as.data.frame(EPI)
ERegion <- EPIRegion[!is.na(EPIRegion)]
hist(ERegion)
hist(ERegion, seq(30, 95, 1.0), probability = TRUE)

# Practice 3 - GPW3_GRUMP
GPW3 <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
attach(GPW3)
n <- is.na(Num.Settlement.Points)
n2 <- Num.Settlement.Points[!n]
n2
summary(Num.Settlement.Points)
fivenum(Num.Settlement.Points)
stem(Num.Settlement.Points)
hist(Num.Settlement.Points)
lines(density(Num.Settlement.Points, na.rm = TRUE, bw = 1))
lines(density(Num.Settlement.Points, na.rm = TRUE, bw = "SJ"))
plot(ecdf(Num.Settlement.Points), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(Num.Settlement.Points)
qqline(Num.Settlement.Points)
GPW3_1 <- as.data.frame(Num.Settlement.Points)
GPW_Num <- GPW3_1[!is.na(GPW3_1)]
hist(GPW_Num)
hist(GPW_Num, seq(0, 16000, by = 1000), probability = TRUE)

# Practice 4 - water_treatment
water <- read.csv("water-treatment.csv")
attach(water)
pH <- is.na(PH.P)
pH2 <- PH.P[!pH]
pH2
summary(PH.P)
fivenum(PH.P)
stem(PH.P)
hist(PH.P)
lines(density(PH.P, na.rm = TRUE, bw = 1))
lines(density(PH.P, na.rm = TRUE, bw = "SJ"))
plot(ecdf(PH.P), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(PH.P)
qqline(PH.P)
hist(PH.P, seq(7.3, 8.5, by = 0.1), probability = TRUE)

# Lab 1 - Part 2
# Exercise 1: fitting a distribution beyond histograms
EPI_data <- read.csv("2010EPI_data (2).csv", skip = 1)
attach(EPI_data)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
plot(ecdf(EPI),do.points=TRUE,verticals = TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
DALY
t <- is.na(DALY)
D <- DALY[!t]
D
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(DALY)
qqline(DALY)
AIR_H
f <- is.na(AIR_H)
A <- AIR_H[!f]
A
plot(ecdf(AIR_H), do.points = FALSE, verticals = TRUE)
par(pty = "s")
qqnorm(AIR_H)
qqline(AIR_H)
qqplot(EPI, DALY)
qqplot(DALY, AIR_H)
# Comparing Distributions
boxplot(EPI, DALY)
# Self-Practice
boxplot(DALY, AIR_H)

# Linear basis and least-squares constraints
multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)$coef
# Intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839
# The estimated regression equation Homeowners = 107494.898 - 6656.839*Immigrant
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
library(tidyverse)
newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)
abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

# In-Class Work: ggplot examples
# Creating plots
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
# the above two are same
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() + geom_point()

# Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
# bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

# Creating Histograms using ggplot
# view the distribution of one-dimentional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

# Creating Box-plots using ggplot
# creating box-plot
plot(ToothGrowth$supp, ToothGrowth$len)
# formula syntax
boxplot(len~supp, data = ToothGrowth) # if the value are in same dataset
boxplot(len~supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$len), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()

# Data Visualization Exercise 1
library(ggplot2)
data(diamonds)
head(diamonds)          # look at the data!
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

ggplot(data = diamonds, mapping = aes(color = cut_number(carat, 5), 
                                      x = price)) +geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()

# Data Visualization Exercise 2
library(gcookbook) # for the dataset. pg_mean dataset is avialbe in this library.
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD # there is not entry for Time == 6 ... did you see that ? 
# Time is numeric (continuous)
str(BOD)

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")
# Convert Time to a discrete (categorical) variable with factor() function.
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")
# change the color of the bars and add an outline to the bars
# NOTE: In ggplot2, the default is to use the British spelling, colour, instead of
# the American spelling, color.
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")
# Grouping Bars Together
# You want to group bars together by a second variable.
# In this example we’ll use the cabbage_exp data set, which has two categorical variables,
# Cultivar and Date, and one continuous variable, Weight:
library(gcookbook) # For the data set
library(ggplot2)
cabbage_exp
# We’ll map Date to the x position and map Cultivar to the fill color
# ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position = "dodge")
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")

library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

# Making Bar Graph of Counts
ggplot(diamonds, aes(x=cut)) +geom_bar() # this is equvalent to using geom_bar(stat="bin)
# The diamonds data set has 53,940 rows, each of which represents information about one
data("diamonds")
diamonds
# In this example, the variable on the x-axis is discrete. If we use a continuous variable on
# the x-axis, we’ll get a histogram
ggplot(diamonds,aes(x=carat)) + geom_bar()
# It turns out that in this case, the result is the same as if we had used geom_histogram() instead of geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()
# Using Colors in Bar Graphs. Now we want to use different colors for the bars in our bar graph
# We can do this by using the "fill" asethetic. 
# We’ll use the uspopchange data set for this example. It contains the percentage change
# in population for the US states from 2000 to 2010. We’ll take the top 10 fastest-growing
# states and graph their percentage change. 
# We’ll also color the bars by region (Northeast,South, North Central, or West)
# Taking Top 10 States 
library(gcookbook) # for the dataset
ups <- subset(uspopchange, rank(Change)>40)
ups
# Now we can make the graph, mapping Region to fill
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
# Do an Experiment with followings
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
# The default colors aren’t very appealing, so you may want to set them, using
# scale_fill_brewer() or scale_fill_manual().
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") +
  scale_fill_manual(values=c("#224455","#DDCC33"))
# Coloring Negative and Positive Bars Differently
# You want to use different colors for negative and positive-valued bars.
library(gcookbook)
csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
# changing the color with scale_fill_manual
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
# Adjusting Bar Width and Spacing
# You want to adjust the width of bars and the spacing between them.
# To make the bars narrower or wider, set width in geom_bar(). The default value is 0.9; 
# larger values make the bars wider, and smaller values make the bars narrower
library(gcookbook) # for the datset
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
# Narrow Bars
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
# Wider bars, maximum width = 1
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
# Different bar widths
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))
# Making a Sketched Bar Graph
library(gcookbook) # for the dataset
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))
# Adding Lables to your Graphs
library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
library(ggplot2)
# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)
# make a Cleveland dot plot
#The simplest way to create a dot plot is to use geom_point() function
library(gcookbook) # For the data set
tophit <- tophitters2001[1:25,] # take top 25 top hitters
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))

ggplot(tophit, aes(x=avg, y=reorder(name,avg))) + geom_point(size=2.5, colour="blue") + 
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = twodash))
# Get the names sorted by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn the name into factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey70")+
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey40") +
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space="free_y")

# Data Visualization Exercise 3
library(gapminder)
library(dplyr)
library(ggplot2)
str(gapminder)
China <- gapminder %>% filter(country == "China") %>% head(10)
View(China)
ggplot(data = China, aes(x=year,y=lifeExp)) + geom_point(color = 'red', size = 3) + xlab('Year') +ylab('Life Expectancy')+ggtitle("Life Expectancy in China")+ theme_bw(base_size = 18)
ggplot(data = gapminder, aes(x= year, y=lifeExp, group =country,color =continent)) + geom_line() +xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")+theme_bw()
ggplot(data = gapminder, aes(x= year, y=lifeExp, group=country,color=continent))+geom_line()+theme_bw()+facet_wrap(~continent)+ xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")
ggplot(data = China, aes(x=year,y=gdpPercap))+geom_line()+scale_y_log10(breaks=c(1000,2000,3000,4000,5000),labels=scales::dollar)+xlim(1940,2010)+theme_gray(base_size = 20)
