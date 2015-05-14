setwd("C:/Users/Luke/Dropbox/FLAME/Data/July04_2014_data/laser_test")

#Set path to where all the laser files are

LaserPath <- list.files(path = "C:/Users/Luke/Dropbox/FLAME/Data/July04_2014_data/laser_test") 

#Bind all files in above path using read.table - Need to specify skip, header, etc. 

LaserFull<-do.call("rbind", lapply(LaserPath, read.table, sep="," ,skip=1,  header = TRUE, fill=TRUE)) 
LaserFull$Time=as.POSIXct(LaserFull$Time, tz="America/Chicago", format="%m/%d/%y %H:%M:%S")

str(LaserFull)
summary(LaserFull$Time)

#Remove Nas

LaserClean<-subset(LaserFull, is.na(LaserFull$Time)==FALSE)
summary(LaserClean$Time)
