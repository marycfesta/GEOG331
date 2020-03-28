###### SET UP SPATIAL PACKAGES ######

# install packages
# install.packages(c("raster","sp","rgdal","rgeos","plyr"))

# load packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

###### READING IN VECTOR DATA ######

#read in shapefiles
#readOGR in rgdal does this
# g1966 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
g1966 <- readOGR("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_1966.shp")

#let's investigate this data
plot(g1966, col="lightblue", axes=TRUE)
str(g1966)
#reference polygons
g1966@polygons[[1]]
#reference more individual polygons
g1966@polygons[[1]]@Polygons
#give crs
g1966@proj4string

#read in shapefiles
# g1998 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
# g2005 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
# g2015 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

g1998 <- readOGR("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_2015.shp")

#investigate format of data
str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

#for a vector object, you can find the projection info in an object called proj4string
g1966@proj4string

#ssplot allows you to map vector data and show different colors for a data value
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

###### WORKING WITH RASTER DATA ######

#### MAPPING SATELLITE IMAGERY ####

#read in rgb imagery from landsat
# redL <- raster("Y:\\Students\\mfesta\\a06\\glacier_09_05_14\\l08_red.tif")
# greenL <- raster("Y:\\Students\\mfesta\\a06\\glacier_09_05_14\\l08_green.tif")
# blueL <- raster("Y:\\Students\\mfesta\\a06\\glacier_09_05_14\\l08_blue.tif")

redL <- raster("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/glacier_09_05_14/l08_blue.tif")


#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#subset the plot using ext argument to zoom in closer on a few glaciers
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#### WORKING WITH RASTER DATA ####

#set up years to read in, 2003 through 2006
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  # NDVIraster[[i]] <- raster(paste0("Y:\\Students\\hkropp\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  NDVIraster[[i]] <- raster(paste0("/Users/maryfesta/Documents/Colgate/Academics/Environmental Data Science/GEOG331/activity6/a06/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

#look at the first year to see what's in a single raster function
str(NDVIraster[[1]])

#get projection, equal area coordinate system
NDVIraster[[1]]@crs

#plot the NDVI data for 2003
plot(NDVIraster[[1]])

#### QUESTION 3 ####
par(mfrow=c(1,2))
plot(NDVIraster[[1]], axes=TRUE)
plot(g1966, axes=TRUE)
# end q3

###### VECTOR DATA ANALYSIS: GLACIER RETREAT ######

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#### QUESTION 4 ####
par(mfrow=c(1,1), mai=c(1,1,1,1))
plot(NDVIraster[[13]], axes=FALSE)
plot(g2015p, add=TRUE, col=NA, border="black")
# end q4

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join data into a table not associated with the shapefile using join from plyr
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#plot the area for each glacier using this table
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   


#### QUESTION 5 ####

# calculate % change between 1966 and 2015
# create new column with percent change in these areas to same
# dataframe that those areas are in
area1966 <- g1966p@data$a1966m.sq
area2015 <- g2015p@data$a2015m.sq
g2015p@data$percentChange <- (abs(area2015 - area1966) / area1966)*100

spplot(g2015p, "percentChange")
# end q5

#make a polygon that shows the difference in glaciers between 2015 and 1966
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#### QUESTION 6 ####

# calculate % loss (same as before but don't take absolute value)
g2015p@data$percentLoss <- ((area1966 - area2015) / area1966)*100
# find the highest % loss
highestLoss <- max(g2015p@data$percentLoss)
# find the glacier with this highest % loss
highestLossName <- g2015p@data$GLACNAME[g2015p@data$percentLoss == highestLoss]

# make a map that best displays the glacial extent for all years for that glacier with the highest % loss
# add a map title that includes the % loss and glacier name


# end q6

##### RASTER DATA ANALYSIS: DOES MORE VEGETATION GROW WITH GLACIAL RETREAT? #####

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)