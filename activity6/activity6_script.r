###### SET UP SPATIAL PACKAGES ######

# install packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

# load packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

###### READING IN VECTOR DATA ######

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

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
g1998 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\mfesta\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

#investigate format of data
str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

#for a vector object, you can find the projection info in an object called proj4string
g1966@proj4string
