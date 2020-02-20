##### TESTING YOUR CODE #####

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
#R doesn't have a built-in assert function like Python
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

##### UNDERSTANDING THE NATURE OF YOUR QA/QC #####

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
# datW <- read.csv("y:\\Students\\mfesta\\a03\\bewkes_weather.csv",
#                  na.strings=c("#N/A"), skip=3, header=FALSE)
######## UNCOMMENT THIS

datW <- read.csv("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity3/a03/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevent units

######## UNCOMMENT THIS

# sensorInfo <-   read.csv("y:\\Students\\mfesta\\a03\\bewkes_weather.csv",
#                          na.strings=c("#N/A"), nrows=2)
sensorInfo <-   read.csv("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity3/a03/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

##### DATA QA/QC #####

### USING PACKAGES ###

#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.

#have to use this every time you want to use a package for your code
#loads a package to the R working environment
library(lubridate)

### WORKING WITH DATES ###

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

### CHECKING MISSING DATA ###

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

###### SETTING UP TESTS FOR QA/QC ######

### VISUAL CHECKS ###

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularily confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

### REALISTIC VALUES ###

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

##### MEASUREMENTS OUTSIDE OF SENSOR CAPABILITIES #####

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

### QUESTION 5 ###
# test if the length of lightscale is the same as that of precipitation
# if the same, graph is able to be plotted successfully
assert(length(lightscale) == length(datW$precipitation), "length is not equal")
assert(length(lightscale) == length(datW$lightning.acvitivy), "length is not equal")
# end q5

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

### QUESTION 6 ###
# filter out suspect wind speed measurements
# remove wind speeds less than 0 and greater than 1
datW$wind.speedQ1 <- ifelse(datW$wind.speed  < 0, NA,
                          ifelse(datW$wind.speed > 1, NA, datW$wind.speed))

# test that the outcome was as expected
# see if all below 0 or above 1 are NA?
assert(length(datW$wind.speedQ1[!is.na(datW$wind.speedQ1[datW$wind.speed < 0])]) <= 0, "Values below 0 exist. Not successful.")
assert(length(datW$wind.speedQ1[!is.na(datW$wind.speedQ1[datW$wind.speed > 1])]) <= 0, "Values above 1 exist. Not successful.")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", xlab = "Day of Year",
     ylab="Wind Speed")

# end q6

### QUESTION 7 ###
# plot soil moisture by day of year
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture/Precipitation",
     type="n")
#plot soil moisture by day of year
points(datW$DD, datW$soil.moisture,
       col= "dark golden rod", pch=15)        

# normalize air temp to match soil moisture
# remove na values from moisture since it is missing some
datW$precip.soil <- (max(datW$soil.moisture[!is.na(datW$soil.moisture)])/max(datW$precipitation)) * datW$precipitation

#plot precipitation points only when there is precipitation     
points(datW$DD[datW$precip.soil > 0], datW$precip.soil[datW$precip.soil > 0],
       col= "navy", pch=19)

# plot soil temperature by day of year
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature/Air Temperature",
     type="n")
#plot soil temp by day of year
points(datW$DD, datW$soil.temp,
       col= "tomato3", pch=15)        

# normalize air temp to match soil temperature
# remove na values in soil temperature as it is missing measurements
datW$air.soil <- (max(datW$soil.temp[!is.na(datW$soil.temp)])/max(datW$air.temperature)) * datW$air.temperature

#plot precipitation points only when there is precipitation     
points(datW$DD, datW$air.soil,
       col= "navy", pch=19)
# end q7

### QUESTION 8 ###
# average air temp, wind speed, soil moisture, soil temp
avg_air_temp <- mean(datW$air.temperature, na.rm=TRUE)
avg_wind_speed <- mean(datW$wind.speed, na.rm=TRUE)
avg_soil_moisture <- mean(datW$soil.moisture, na.rm=TRUE)
avg_soil_temp <- mean(datW$soil.temp, na.rm=TRUE)

# total precipitation
total_prcp <- sum(datW$precipitation, na.rm=TRUE)

# number of calculations that went into each
length(datW$air.temperature[!is.na(datW$air.temperature)])
length(datW$wind.speed[!is.na(datW$wind.speed)])
length(datW$soil.moisture[!is.na(datW$soil.moisture)])
length(datW$soil.temp[!is.na(datW$soil.temp)])
length(datW$precipitation[!is.na(datW$precipitation)])



# total precipitation
# end q8

### QUESTION 9 ###
# put all in same window 
par(mfrow=c(2,2))

# soil moisture plot
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture")

# air temperature plot
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temperature (Degrees C)")

# soil temperature plot
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (Degrees C)")

# precipitation plot
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation")

# end q9