#Heat_Maps(shapefile)
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

#Heat_Maps(shapefile, getwd(), Site, Date)

Heat_Maps<-function(shape, destination, Site, Date){
  #Create subfolder 'maps' if it does not already exist
  folders<-list.files(path = paste(getwd(), "", sep=""))
  if(length(folders[folders=="maps"])==0){
    dir.create(paste(getwd(), "/maps", sep=""))}
  
  B<-100 #Number of color breaks
  colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

  #Load Mendota basemap if site==Mendota
  if(Site=="LakeMendota"){
    base<-Mendota_Base
  for (i in 2:length(shape@data)){
    if (is.numeric(shape@data[,i])==TRUE){
    name<-names(shape@data)[i]
    a<-shape[is.na(shape@data[,i])==FALSE,]
    a$Col <- as.numeric(cut(a@data[,i],breaks = B))
    a$Color<-colors[a$Col]
  
    png(paste(destination, "/maps/", name, Site, Date, ".png", sep=""), res=600, width=6,height=10, units="in")
    
    par(mar=c(2,1.5,2,0),bg=NA)
    par(mfrow=c(2,1)) 
    par( oma = c( 1.5,2,1,0 ) )
    par(tck=-.02)
    par(mgp=c(0,.6,0))
    par(las=0)
    
    plot(a, col=a$Color, pch=16)
    plot(base, add=TRUE)
   
    axis(1)
    axis(2)
    mtext("Latitude", 2, 2)
    mtext("Longitude", 1, 2)
    box(which='plot')
    
    plot(a@data[,i]~a$ltime, col=a$Color, pch=16, yaxt="n", ylab="", xlab="")
    #axis(1)
    axis(2)
    mtext(paste(name), 2, 2)
    mtext("Time", 1, 2)   
    mtext(paste(Site, Date, name, sep=" "), 3, -1, outer=TRUE, cex=1.5)
    dev.off()
  }
}
}

if(Site=="Pool8"){
  base<-Pool8_Base
  base_Lat<-spTransform(Pool8_Base, CRS("+init=epsg:4326"))
  z<-c()
  zonecolors<-c("gray70", "gray70", "gray45", "gray45", "lightcyan2", "lightcyan1", "lightcyan1", "blanchedalmond")
  AQAs<-c(0, 1504, 1502,1503, 1520, 1510, 1530, 1500)
  frame <- as.data.frame(cbind(zonecolors, AQAs))
  
  for (i in 1:length(base$AQA)){
    y<-base$AQA[i]
    z[i]<-as.character(frame$zonecolors[frame$AQAs==y])
  }

  for (i in 2:length(shape@data)){
    if (is.numeric(shape@data[,i])==TRUE){
    name<-names(shape@data)[i]
    a<-shape[is.na(shape@data[,i])==FALSE,]
    a$Col <- as.numeric(cut(a@data[,i],breaks = B))
    a$Color<-colors[a$Col]
    
    png(paste(destination, "/maps/", name, Site, Date, ".png", sep=""), res=600, width=10,height=10, units="in")
    
    par(mar=c(2,1.5,2,2),bg="white")
    par(mfrow=c(1,2)) 
    par( oma = c( 1.5,2,1,0 ) )
    par(tck=-.02)
    par(mgp=c(0,.6,0))
    par(las=0)
    
    plot(base_Lat, col=z, border=NA, xaxs="i", yaxs="i")
    plot(a, col=a$Color, pch=16, add=TRUE)
    legend("left", inset=0.01, c("Main Channel", "Side Channel", "Impounded", "Backwater", "Land"), fill=c(zonecolors[3], zonecolors[1], zonecolors[5], zonecolors[6], zonecolors[8]), bty="n")
    
    
    axis(1)
    axis(2)
    mtext("Latitude", 2, 2)
    mtext("Longitude", 1, 2)
    box(which='plot')
    
    plot(a@data[,i]~a$ltime, col=a$Color, pch=16, yaxt="n", ylab="", xlab="")
    #axis(1)
    axis(2)
    mtext(paste(name), 2, 2)
    mtext("Time", 1, 2)   
    mtext(paste(Site, Date, name, sep=" "), 3, -1, outer=TRUE, cex=1.5)
    
    dev.off()
  }
  }
}

#No basemap if Site!=Mendota or Pool8
if(Site!="LakeMendota" & Site!="Pool8"){

  for (i in 2:length(shape@data)){
    if (is.numeric(shape@data[,i])==TRUE){
    name<-names(shape@data)[i]
    a<-shape[is.na(shape@data[,i])==FALSE,]
    a$Col <- as.numeric(cut(a@data[,i],breaks = B))
    a$Color<-colors[a$Col]
    
    png(paste(destination, "/maps/", name, Site, Date, ".png", sep=""), res=600, width=6,height=10, units="in")
    
    par(mar=c(2,1.5,2,0),bg=NA)
    par(mfrow=c(2,1)) 
    par( oma = c( 1.5,2,1,0 ) )
    par(tck=-.02)
    par(mgp=c(0,.6,0))
    par(las=0)
    
    plot(a, col=a$Color, pch=16)
    
    axis(1)
    axis(2)
    mtext("Latitude", 2, 2)
    mtext("Longitude", 1, 2)
    box(which='plot')
    
    plot(a@data[,i]~a$ltime, col=a$Color, pch=16, yaxt="n", ylab="", xlab="")
    #axis(1)
    axis(2)
    mtext(paste(name), 2, 2)
    mtext("Time", 1, 2)   
    mtext(paste(Site, Date, name, sep=" "), 3, -1, outer=TRUE, cex=1.5)
    dev.off()
  }
  }
}




}




