#working code to create raster from point data and calculate geostatistics for FLAMe
#JTC, June 2015
###################################################################################
setwd("C:/Users/jtcrawford/Dropbox/FLAME/Data/2015-05-22_LakeMendota/shapefiles")
library(rgdal)
shapef = readOGR(".", "LakeMendota2015-05-22")
###################################################################################
crds = coordinates(shapef)
poly = crds[chull(crds), ]
poly = rbind(poly, poly[1, ])
SPpoly = SpatialPolygons(list(Polygons(list(Polygon(poly)), ID = "poly")))
bbox(shapef)
#(apply(bbox(shapef), 1, diff)%/%50) + 1#not sure what this does
grd <- GridTopology(c(178600, 330300), c(50, 50), c(48, 41))# this needs to be automated?
SG <- SpatialGrid(grd)
inside <- over(SG, SPpoly)
SGDF <- SpatialGridDataFrame(grd, data = data.frame(list(ins = inside)))
SPDF <- as(SGDF, "SpatialPixelsDataFrame")#error here
###################################################################################
plot(shapef, axes = TRUE)
plot(SPpoly, add = TRUE)
plot(SPDF, col = "red", add = TRUE)
