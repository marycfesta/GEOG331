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
g2015p@data$percentChange <- ((area2015 - area1966) / abs(area1966))*100

spplot(g2015p, "percentChange")
# end q5

#make a polygon that shows the difference in glaciers between 2015 and 1966
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#### QUESTION 6 ####

# find the highest % loss (most negative percent change)
highestLoss <- min(g2015p@data$percentChange)
# find the glacier with this highest % loss
highestLoss2015 <- subset(g2015p, g2015p$percentChange == highestLoss)
# highestLossName <- g2015p@data$GLACNAME[g2015p@data$percentLoss == highestLoss]
highestLossName <- highestLoss2015$GLACNAME

# subset remaining years with the highest loss glacier
highestLoss1966 <- subset(g1966p, g1966p$GLACNAME == highestLossName)
highestLoss1998 <- subset(g1998p, g1998p$GLACNAME == highestLossName)
highestLoss2005 <- subset(g2005p, g2005p$GLACNAME == highestLossName)

# make a map that best displays the glacial extent for all years for that glacier with the highest % loss
# add a map title that includes the % loss and glacier name
par(mai = c(1,1,1,1))
plot(NDVIraster[[13]], axes=FALSE, box=FALSE, xlim=c(-80250,-78700), ylim=c(106700,107400))
plot(highestLoss2015, col=NA, border="black", add=TRUE)
plot(highestLoss2005, col=NA, border="slateblue3", add=TRUE)
plot(highestLoss1998, col=NA, border="steelblue2", add=TRUE)
plot(highestLoss1966, col=NA, border="coral2", add=TRUE)
title("Boulder Glacier - 84.72% Loss")
legend("bottomleft", box.lty=0, lty=1,
       legend=c("1966","1998","2005","2015"),
       col=c("coral2","steelblue2","slateblue3","black"))


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

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

#### QUESTION 9 ####

# remove the no glacier zone
meanChangeNoZero <- meanChange[-1,]
# add mean change to the 2015 glaciers
g2015p@data$meanChange <- meanChangeNoZero[,2]
# plot the mean change within the 2015 glacier polygons
spplot(g2015p, "meanChange")

# end q9

#### QUESTION 11 ####

# make a raster dataset for the average maximum NDVI across all years
avgNDVIraster <- calc(NDVIstack, mean)
plot(avgNDVIraster, axes=FALSE)

# assign colors to each glacier based on glacier size
summary(area2015) # get quartiles
g2015p@data$NDVIcol <- ifelse(g2015p@data$a2015m.sq <= 80653,"deepskyblue4",
                              ifelse(g2015p@data$a2015m.sq <= 218317,"deepskyblue1",
                                     ifelse(g2015p@data$a2015m.sq <= 503913,"darkorchid1","darkorchid4")))
# plot the glaciers on top of the NDVI
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
# add a title and legend
title("Average Maximum NDVI and Glacier Size")
legend("bottomleft", box.lty=0, lty=1, lwd=5,
       legend=c("22084-80653","80654-218317","80653-503913","503914-1656043"),
       col=c("deepskyblue4","deepskyblue1","darkorchid1","darkorchid4"))

# end q11
