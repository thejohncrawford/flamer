############################################################################################
rm(list=ls(all=TRUE)) #clear the environment
#Change directory to match desired dataset
FLAMEsubdirectory<-("/Dropbox/FLAME/Data/2015-04-14_LakeMendota")

library(rgdal)
library(sp)

# ==============================
# Don't change anything below!
# ==============================

#point to the correct directory with the FLAMe scripts
setwd("C:/Users/jtcrawford/Dropbox/FLAME/R scripts")
setwd("C:/Users/Luke/Dropbox/FLAME/R scripts")
setwd("C:/Users/Vince/Dropbox/FLAME/R scripts")
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

Mendota_Base<-readOGR(getwd(), "Mendota_shape")
#Pool8_Base<-readOGR(getwd(), "Pool8")
#Add other common basemaps

#point to the data folder; individual instrument files need to be in subfolders 
#with the names 'laser', 'gps', 'ysi' 'nitro', 'meta', 'maps', 'shapefiles' (case sensitive)
setwd(paste("C:/Users/Luke", FLAMEsubdirectory, sep=""))
setwd(paste("C:/Users/jtcrawford", FLAMEsubdirectory, sep=""))
setwd(paste("C:/Users/Vince", FLAMEsubdirectory, sep=""))
#read in a metadata file that indicates the name, date (YYYY-MM-DD !!!), site, flame intervals, and water samples
MetaPath <- list.files(path = paste(getwd(), "/meta", sep=""))
meta<-do.call("rbind", lapply(paste("meta/", MetaPath, sep=""), read.table, sep="," ,skip=1,  header = TRUE, fill=TRUE)) 
Date<-as.Date(meta$Date[1], format="%m/%d/%Y")
Site<-as.character(meta$Site[1])
Times<-subset(meta, is.na(as.POSIXct(Flame_On, format="%H:%M:%S"))==FALSE)
Flame_On <- as.POSIXct(paste(Date, Times$Flame_On, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")
Flame_Off <- as.POSIXct(paste(Date, Times$Flame_Off, sep=" "), format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")
########################################################################################
#do all 'dem bad things to the FLAMe data
flame_engage(meta)
