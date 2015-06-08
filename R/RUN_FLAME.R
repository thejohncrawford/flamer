############################################################################################
rm(list=ls(all=TRUE)) #clear the environment
# Change directory to match desired dataset
# Change date and sitename!
FLAMEsubdirectory<-("/Dropbox/FLAME/Data/YYYY-MM-DD_SiteName")

# ==============================
# Don't change anything below!
# ==============================

library(sp)
library(rgdal)

#point to the correct directory with the FLAMe scripts
setwd("C:/Users/jtcrawford/Dropbox/FLAME/R scripts")
setwd("C:/Users/Luke/Dropbox/FLAME/R scripts")
setwd("C:/Users/Vince/Dropbox/FLAME/R scripts")
setwd("E:/Dropbox/FLAME/R scripts")

#call the FLAMe scripts
source("suna_datetime.R")
source("read_instruments.R")
source("tau_correct.R")
source("write_shapefile.R")
source("convert_gases.R")
source("Flame_Wrapper.R")
source("Heat_Maps.R")
source("extract_flame.R")

#Load all basemaps
setwd("C:/Users/jtcrawford/Dropbox/FLAME/basemaps")
setwd("C:/Users/Luke/Dropbox/FLAME/basemaps")
setwd("C:/Users/Vince/Dropbox/FLAME/basemaps")
setwd("E:/Dropbox/FLAME/basemaps")

Mendota_Base<-readOGR(getwd(), "Mendota_shape")
Pool8_Base<-readOGR(getwd(), "Pool8")
#Add other common basemaps

#point to the data folder; individual instrument files need to be in subfolders 
#with the names 'laser', 'gps', 'ysi' 'nitro', 'meta', 'maps', 'shapefiles' (case sensitive)
setwd(paste("C:/Users/Luke", FLAMEsubdirectory, sep=""))
setwd(paste("C:/Users/jtcrawford", FLAMEsubdirectory, sep=""))
setwd(paste("C:/Users/Vince", FLAMEsubdirectory, sep=""))
setwd(paste("E:", FLAMEsubdirectory, sep=""))

#read in a metadata file that indicates the name, date (YYYY-MM-DD !!!), site, flame intervals, and water samples
SamplesPath <- list.files(path = paste(getwd(), "/samples", sep=""))
Samples<-do.call("rbind", lapply(paste("samples/", SamplesPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE)) 
MetaPath <- list.files(path = paste(getwd(), "/meta", sep=""))
meta<-do.call("rbind", lapply(paste("meta/", MetaPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE)) 
Date<-as.Date(meta$Date[1], format="%Y-%m-%d")
Site<-as.character(meta$Site[1])
Times<-subset(meta, is.na(as.POSIXct(Flame_on, format="%H:%M:%S"))==FALSE)
Flame_On <- as.POSIXct(paste(Date, Times$Flame_on, sep=" "), format="%Y-%m-%d %H:%M:%S", tz=as.character(meta$GPS_Timezone[1]))
Flame_Off <- as.POSIXct(paste(Date, Times$Flame_off, sep=" "), format="%Y-%m-%d %H:%M:%S",  tz=as.character(meta$GPS_Timezone[1]))
########################################################################################
#do all 'dem bad things to the FLAMe data
flame_engage(meta)
