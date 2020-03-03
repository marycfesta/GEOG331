###### ACTIVITY 5 - MARY FESTA #####

### general notes
# Eqp used when equipment fails instead of NA
# USGS includes a quality data flag
# urban hydrographs higher bc of fewer permeable surfaces

##### WORKING WITH USGS STREAMFLOW DATA #####

#load in lubridate
library(lubridate)

#read in streamflow data
# datH <- read.csv("y:\\Students\\mfesta\\a05\\stream_flow_data.csv",
#                  na.strings = c("Eqp"))
# CHANGE BACK BEFORE SUBMIT
datH <- read.csv("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity5/a05/stream_flow_data.csv",
                 na.strings = c("Eqp"))


head(datH)         

# #read in precipitation data
# #hourly precipitation is in mm
# datP <- read.csv("y:\\Students\\mfesta\\a05\\2049867.csv")   

# CHANGE BACK BEFORE SUBMIT
datP <- read.csv("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity5/a05/2049867.csv")   

head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))          

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#### QUESTION 3 ####

# look at timestamps and see how frequently they're taken to see freq of observations
# how frequently is the usgs trying to collect this data

# check how many observations are in the reliable streamflow data
str(datD)
# check how many observations are in the precipitation data
str(datP)
# end q3

##### BASIC PLOT FORMATTING #####

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

## ADD A POLYGON

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

## ADJUST AXIS DISPLAY

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

## ADD A LEGEND

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

## ADJUST THE LEGEND

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#### QUESTION 5 ####
# add a line that shows observations for 2017 onto the graph of the average
# change the x axis label to show each month instead of doy
# make the 2017 line a different color

#start new plot
dev.new(width=8,height=8)

month_nums <- month(datesD)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
# create a variable to hold the discharge from 2017
discharge17 <- datD$discharge[datD$year==2017]
# create a variable to hold doy data from 2017
doy17 <- datD$doy[datD$year==2017]
        
#plot the 2017 data
lines(doy17, discharge17, col="red")

axis(1, at=c(1,32,60,91,121,152,182,213,244,274,305,335), #tick intervals
     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017 data"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")#no legend border
# end q5

#### QUESTION 6 ####

# calculate standard deviation for 2017
sd17 <- aggregate(discharge17, by=list(doy17), FUN="sd")
colnames(sd17) <- c("doy","dailySD")

# calculate mean and standard deviation for 2017
sd2017 <- sd(discharge17)
mean2017 <- mean(discharge17)

# end q6

##### MAKING A HYDROGRAPH #####

#### QUESTION 7 ####

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

# indicate days with 24 hours of precipitation measurements
with24hrs <- aggregate(datP$doy, by=list(datP$doy, datP$year), FUN="length")

# create a dataframe for days with 24 hours of precipitation measurements
precip24hrs <- data.frame(with24hrs)
# rename columns
names(precip24hrs) <- c("doy", "year", "precipMeas")

#calculate a decimal year, but account for leap year
precip24hrs$decYear <- ifelse(leap_year(precip24hrs$year),precip24hrs$year + ((precip24hrs$doy-1)/366),
                       precip24hrs$year + ((precip24hrs$doy-1)/365))          

# what to put for discharge here?
points(precip24hrs$decYear[precip24hrs$precipMeas == 24], datP$discharge[precip24hrs$decYear], col="red")
# end q7

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#### QUESTION 8 ####

# end q8

##### MAKING BOX PLOTS AND VIOLIN PLOTS #####

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

#### QUESTION 9 ####

# violin plot for 2016
#specify year as a factor
dat16 <- data.frame(as.factor(datD$year)[datD$year==2016])
dat16$y16 <- datD$year[datD$year==2016]
dat16$discharge <- datD$discharge[datD$year==2016]

#make a violin plot
ggplot(data= dat16, aes(y16,discharge)) + 
        geom_violin() + labs(x = "2016")

# meteorological seasons
# spring: mar 1 - may 31 (60 - 151)
# summer: jun 1 - aug 31 (152 - 243)
# autumn: sep 1 - nov 30 (244 - 334)
# winter: dec 1 - feb 28 (335 - 59)
# add one to all for leap year
datD$season <- ifelse()

# violin plot for 2017
#specify year as a factor
dat17 <- data.frame(as.factor(datD$year)[datD$year==2017])
dat17$y17 <- datD$year[datD$year==2017]
dat17$discharge <- datD$discharge[datD$year==2017]

#make a violin plot
ggplot(data= dat17, aes(y17,discharge)) + 
        geom_violin() + labs(x = "2017")

# end q9
