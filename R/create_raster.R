#working code to create raster from point data and calculate geostatistics for FLAMe
#JTC, June 2015
#following example from http://www.bias-project.org.uk/ASDARcourse/unit4_slides.pdf
###################################################################################
setwd("C:/Users/jtcrawford/Dropbox/FLAME/Data/2015-05-22_LakeMendota/shapefiles")
library(rgdal)
library(gstat)
shapef = readOGR(".", "LakeMendota2015-05-22")
str(shapef)
shapef = spTransform(shapef, CRS("+init=epsg:32616"))
str(shapef)
###################################################################################
#the "roll-your-own" boundaries for interpolation
crds = coordinates(shapef)
poly = crds[chull(crds), ]
poly = rbind(poly, poly[1, ])
SPpoly = SpatialPolygons(list(Polygons(list(Polygon(poly)), ID = "poly")))
bbox(shapef)# extract coords.x1 and coords.x2 min as the first arguments to GridTopology
(apply(bbox(shapef), 1, diff)%/%50) + 1#these values become the last argument to GridTopology
grd <- GridTopology(c(297976, 4772269.9), c(100, 100), c(175, 150))# this needs to be automated, need to work with UTM's 
SG <- SpatialGrid(grd)
inside <- over(SG, SPpoly)
SGDF <- SpatialGridDataFrame(grd, data = data.frame(list(ins = inside)))
SPDF <- as(SGDF, "SpatialPixelsDataFrame")
###################################################################################
#plot the original shapefile, add the convex hull polygon, add the grid for interpolation
plot(shapef, axes = TRUE)
plot(SPpoly, add = TRUE)
plot(SPDF, col = "red", add = TRUE)
####################################################################################
#Setting up class intervals and palettes initially will save time later;
#note the use of colorRampPalette, which can also be specified
#from RColorBrewer palettes
bluepal <- colorRampPalette(c("azure1", "steelblue4"))
brks <- c(0, 130, 155, 195, 250, 330, 450, 630, 890, 1270, 1850)
cols <- bluepal(length(brks) - 1)
sepal <- colorRampPalette(c("peachpuff1", "tomato3"))
brks.se <- c(0, 240, 250, 260, 270, 280, 290, 300, 350, 400, 1000)
cols.se <- sepal(length(brks.se) - 1)
scols <- c("green", "red")
###################################################################################
