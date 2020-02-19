# install packages
# install.packages(c("ggplot2","dplyr"))

#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length


# versicolor contains iris data only for species versicolor
# include all columns, so leave a ", "
versicolor <- iris[iris$Species=="versicolor", ]

# create vectors for the x and y variables, making sure indices correspond correctly
x <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y <- c("Sepal.Width", "Petal.Width", "Petal.Length")

# declare variables for the for loop
# empty list for lm.out, will end up with a list of lists
lm.out <- list()

# create for loop, iterating from 1 to 3
# in many cases you may need the paste function e.g. paste(y[i])
for ( i in 1:3 ){
  # double brackets to index lm.out because it is a list of lists
  lm.out[[i]] <- lm(versicolor[,y[i]] ~ versicolor[,x[i]])
}

# lm is used to fit linear models, can be used to carry out regression
# left of ~ is dependent variable, y ~ x
# dependent variable "is a function of" independent variable
# subset by a specific column
# empty space at beginning because you're taking all rows
# example:
# lm.out <- lm(versicolor[,"Sepal.Width"] ~ versicolor[,"Sepal.Length"])


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

# iris left
# height right
# want to join height into iris (normalize petal measurements by height)
iris2 <- left_join(iris,height,by="Species")
# normalized petal width
iris2$Petal.Width/iris2$Height.cm


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
# aes is aesthetics
# + means run the two functions simultaneously
ggplot(data=iris, aes(Sepal.Length,Sepal.Width)) +
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
# add arguments about the theme
ggplot(data=iris, aes(Sepal.Length,Sepal.Width)) +
  geom_point() +
  theme_classic()

#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size
ggplot(data=iris, aes(Sepal.Length,Sepal.Width, color=Species)) +
  geom_point(size=4) +
  theme_classic()


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		