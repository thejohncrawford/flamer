
# Code to plot side by side aquatic areas and nitrate concentration from pool 8 Aug 4, 2015
# Committed to github Sept 2016

# shape = shapefile
# base = pool 8 aquatic areas basemap

# Don't need all these. find the important ones!
library(maptools)
library(rgdal)
library(sp)
library(gstat)
library(raster)
library(geoR)
library(automap)
library(ggplot2)
library(colorRamps)
library(squash)
library(caTools)
library(kriging)
library(RgoogleMaps)
library(GISTools)


z<-c()
zonecolors<-c("gray70", "gray70", "gray45", "gray45", "lightcyan3","lightcyan2",  "lightcyan1", "blanchedalmond")
AQAs<-c(0, 1504, 1502,1503, 1520, 1510, 1530, 1500)
frame <- as.data.frame(cbind(zonecolors, AQAs))

for (i in 1:length(base$AQA)){
  y<-base$AQA[i]
  z[i]<-as.character(frame$zonecolors[frame$AQAs==y])
}
i=18
for (i in 2:length(shape@data)){
  if (is.numeric(shape@data[,i])==TRUE){
    name<-names(shape@data)[i]
    a<-shape[is.na(shape@data[,i])==FALSE,]
    # a$Col <- as.numeric(cut(a@data[,i],breaks = B))
    breaks <- 10^(seq(log10(.17),log10(4.69), length.out=B))
    a$Col <- as.numeric(cut(a@data[,i],breaks = breaks, length.out=B))
    a$Color<-colors[a$Col]
    
    #     png(paste(destination, "/maps/", name, Site, Date, ".png", sep=""), res=600, width=10,height=10, units="in")
    png(paste("E:/Dropbox/FLAME/Data/2015-08-04_UMR_Day4", "/maps/", "NitrateAquaticArea.png", sep=""), res=600, width=7,height=10, units="in")
    par(mar=c(2,1.5,2,1),bg="white")
    par(mfrow=c(1,2)) 
    par( oma = c( 0,2,0,0 ) )
    par(tck=-.02)
    par(mgp=c(0,.6,0))
    par(las=0)
    
    layout(matrix(c(1,2,3,4), nrow=2, ncol=2), widths=c(5,5), heights=c(9,1))
    
    plot(base, col=z, border=NA, xaxs="i", yaxs="i")
    
    axis(1)
    axis(2)
    mtext("Latitude", 2, 2)
    mtext("Longitude", 1, 2)
    mtext("Pool 8 Aquatic Areas", 3, 0.5)
    north.arrow(-91.3, 43.6,len=.005, lab="N")
    map.scale(x=-91.3, y=43.585, len=0.05, units="km", ndiv=4)
    
    box(which='plot')
    
    
    par(mar=c(0,0,0,0), bg=NA)
    plot(base, col=NA, border=NA)
    legend("center",  c("Land","Main Channel", "Side Channel", "Impounded", "Contiguous Backwater","Isolated Backwater"), fill=c(zonecolors[8],zonecolors[3], zonecolors[1], zonecolors[5], zonecolors[6], zonecolors[7]), bty="n", ncol=2)
    
    par(mar=c(2,1.5,2,1),bg="white")
    plot(base, col=z, border=NA, xaxs="i", yaxs="i")
    plot(a, col=a$Color, pch=16, add=TRUE)
    
    mtext("Nitrate", 3, 0.5)
    box(which='plot')
    
    par(mar=c(4,1.5,0,1), bg=NA)
    
    image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1)
    mtext(expression(paste('Nitrate (mg N ', "L"^"-1",  ")")), 1, 2.5, cex=1)
    
    
    dev.off()
  