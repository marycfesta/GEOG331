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

# get more information about the dataframe
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))