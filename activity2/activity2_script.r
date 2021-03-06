# questions are labeled with ### QUESTION [NUMBER] ###
# e.g. ### QUESTION 1 ###

### VECTORS ###

# make a vector of tree heights in meters
heights <- c(30,41,20,22)
# convert to cm
heights_cm <- heights*100
heights_cm

# look at the first tree height
heights[1]

# look at the 2nd and 3rd tree heights
heights[2:3]

### MATRICES ###

# get more info on the matrix function
help(matrix)

# set up a matrix with 2 columns and fill in by rows
# first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

# set up a matrix that fills in by columns
# first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

# subset the matrix to look at row 1, column2
Mat.bycol[1,2]

# Look at all values in row 1
Mat.bycol[1,]

# look at all values in column 2
Mat.bycol[,2]

### DATAFRAMES ###

# read in weather station file from the data folder
datW <- read.csv("y:\\Students\\mfesta\\a02\\2011124.csv")
#change back to original filepath to submit
#datW <- read.csv("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity2/a02/2011124.csv")

# get more information about the dataframe
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

### QUESTION 2 ###

# create character data vector
cvect <- as.character(c("this", "is", "a", "character", "vector"))
# check if it's a character vector
is.character(cvect)

# create numeric data vector
nvect <- as.numeric(c(1,2,3,4,5))
# check if it's numeric
is.numeric(nvect)

# create integer data vector
intvect <- as.integer(c(1,2,3,4,5))
# check if it's integer
is.integer(intvect)

# create factor data vector
fvect <- as.factor(c("this", "is", "a", "factor", "vector"))
# check if it's a factor
is.factor(fvect)

### DESCRIPTIVE STATISTICS AND HISTOGRAMS ###

#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

### QUESTION 3 ### 
# help for arguments in hist function
help(hist)
help(paste)
# end q3

#add all histograms into the same window
par(mfrow=c(2,2))

#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

### QUESTION 4 ###
#make a histogram for the second site in our levels, Livermore
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="gold2",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the third site in our levels, Mandan Experiment Station
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="antiquewhite3",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the fourth site in our levels, Mormon Flat
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="goldenrod3",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#end q4

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

### QUESTION 5 ###
#make a histogram for the second site in our levels, Livermore
h2 <- hist(datW$TAVE[datW$siteN == 2],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[2]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,35, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,35, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y.scaled <- (max(h2$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguments are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#make a histogram for the third site in our levels, Mandan Experiment Station
h3 <- hist(datW$TAVE[datW$siteN == 3],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[3]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-40,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-40,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y.scaled <- (max(h3$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguments are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#make a histogram for the fourth site in our levels, 
h4 <- hist(datW$TAVE[datW$siteN == 4],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[4]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(0,40, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(0,40, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y.scaled <- (max(h4$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguments are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)
#end q5

help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm with 5 gives me all probability (area of the curve) below 5 
#subtracting area below 0 gives the probability of temperatures in the range 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm returns the value associated with a probability
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

### QUESTION 6 ###
# determine what unusually high temperatures in Aberdeen would start at
# if the mean were to increase by 4 degrees with the same standard deviation
highThresh <- qnorm(0.95,
              mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
              sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# probability of getting the previously calculated temperature
1 - pnorm(highThresh,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
# end q6

### QUESTION 7 ###

#look at the mean precipitation for Aberdeen
mean(datW$PRCP[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#get the mean for precipitation across all sites
averagePrcp <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)

# histogram of daily precipitation for Aberdeen
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average precipitation (inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE) - sd(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE) + sd(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
# end q7

### QUESTION 8 ###

# get precipitation for each year and site in the data
prcpYearSite <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)
prcpYearSite$siteN <- as.numeric(prcpYearSite$Group.1)

# precipitation by each level (aka each site)
lvl1 <- prcpYearSite$x[prcpYearSite$siteN == 1]
lvl2 <- prcpYearSite$x[prcpYearSite$siteN == 2]
lvl3 <- prcpYearSite$x[prcpYearSite$siteN == 3]
lvl4 <- prcpYearSite$x[prcpYearSite$siteN == 4]
lvl5 <- prcpYearSite$x[prcpYearSite$siteN == 5]

# make a histogram of annual precipitation for Aberdeen
hprcp <- hist(lvl1,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average precipitation (inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(lvl1,na.rm=TRUE),
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(lvl1,na.rm=TRUE) - sd(lvl1,na.rm=TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(lvl1,na.rm=TRUE) + sd(lvl1,na.rm=TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(1000,3000, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(1000,3000, length.out = 100),
                 mean(lvl1,na.rm=TRUE),
                 sd(lvl1,na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y.scaled <- (max(hprcp$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguments are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)
# end q8

### QUESTION 9 ###
#mean annual precipitations
mean(lvl1,na.rm=TRUE)
mean(lvl2,na.rm=TRUE)
mean(lvl3,na.rm=TRUE)
mean(lvl4,na.rm=TRUE)
mean(lvl5,na.rm=TRUE)

#mean average temperatures
mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)
mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE)
mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE)
mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE)
mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE)
# end q9
