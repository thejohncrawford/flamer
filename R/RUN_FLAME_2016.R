############################################################################################
rm(list=ls(all=TRUE)) #clear the environment
 
# ==================================================
# Change directory to match desired dataset
# Change date and sitename!
# Should read ("/Dropbox/.../YYYY-MM-DD_SiteName)
# Added a loop function to do this for all 6 ColumbiaRiver Days. Remove loop if wanting to run a single day.
# ==================================================

# FLAME_Data_Directory<-"Dropbox/FLAME_MississippiRiver/Data/2016-08-03_Pool8"
FLAME_Data_Directory<-"Dropbox/FLAME_NHLDLakes/Data/2016-05-24_NewmanLake"
# FLAME_Data_Directory<-"Dropbox/FLAME_YaharaLakes/Data/2016-09-01_LakeMendota"

# Set picaroo timeoff if needed
# Picarro_Time_Offset<-15

# ==================================================
# Point to the main FLAME dropbox directory
# This should be your "yourcomputer"/Dropbox/FLAME
# ==================================================

setwd("E:/Dropbox/FLAME") #Loken Desk
setwd("C:/Dropbox/FLAME") #Flame thinkpad

# Other computers
# setwd("F:/Dropbox/FLAME") #?
# setwd("C:/Users/jtcrawford/Dropbox/FLAME") #Crawford Laptop
# setwd("C:/Users/Luke/Dropbox/FLAME") #Loken Laptop

# ==============================
# Don't change anything below!
# Script uses relative directories within "Dropbox/FLAME"
# ==============================

setwd("R scripts")

# call the FLAMe scripts
source("FLAMer.R")

# Load tau corrections
setwd("..")
setwd("Taus")

taus2015.01<-read.csv("Manual_Hydros_Taus_2015.csv", header=TRUE)
taus2016.01<-read.csv("Manual_Hydros_Taus_2016.01.csv", header=TRUE)
taus2016.02<-read.csv("Manual_Hydros_Taus_2016.02.csv", header=TRUE)
taus2016.03<-read.csv("Manual_Hydros_Taus_2016.03.csv", header=TRUE)

# Load rule table for sensorQC
setwd("..")
setwd("SensorQC")

ruletable<-read.csv("SensorQCRulesStrick2_2015.csv", header=TRUE, stringsAsFactors=FALSE)

# Add common basemaps
setwd("..")
setwd("basemaps")

Mendota_Base<-readOGR(getwd(), "Mendota_shape")
Mendota_Base_UTM<-spTransform(Mendota_Base, CRS("+proj=utm +zone=15 ellps=WGS84"))
Monona_Base<-readOGR(getwd(), "Monona_shape")
Monona_Base_UTM<-spTransform(Monona_Base, CRS("+proj=utm +zone=15 ellps=WGS84"))
Pool8_Base_UTM<-readOGR(getwd(), "Pool8")
Pool8_Base<-spTransform(Pool8_Base_UTM, CRS("+init=epsg:4326"))
lakes_Base_UTM<-readOGR(getwd(), "regional_lakes_6")
lakes_Base<-spTransform(lakes_Base_UTM, CRS("+init=epsg:4326"))
UNDERC_Base_UTM<-readOGR(getwd(), "Peter_Paul_Tuesday_shifted2")
UNDERC_Base<-spTransform(UNDERC_Base_UTM, CRS("+init=epsg:4326"))

# For new lakes in the NHLD; you can uncomment this code to get more basemaps
# lakes_Base_UTM<-readOGR(getwd(), "NHLD_All2")
# NHLD_Base<-spTransform(lakes_Base_UTM, CRS("+init=epsg:4326"))

# Read in zerotimes from Peter Turner
setwd("..")
setwd("Ambient_Times")

zerotimes<-read.csv("Ambient_Times_2015.csv", header=TRUE)
zerotimes[,1]<-as.POSIXct(strptime(as.character(zerotimes[,1]), format="%m/%d/%Y %H:%M"), tz="America/Chicago")
zerotimes[,2]<-as.POSIXct(strptime(as.character(zerotimes[,2]), format="%m/%d/%Y %H:%M"), tz="America/Chicago")

# point to the data folder; individual instrument files need to be in subfolders 
# with the names 'laser', 'gps', 'ysi' 'nitro', 'meta', 'samples' (case sensitive)

setwd("..")
setwd("..")
setwd("..")
setwd(paste(getwd(), FLAME_Data_Directory, sep=""))

# ===========================================================
# Start Open CR Loop Code
# Loop code, here and an after flame_engage function
# Code will loop through all 13 UMR files and perform {flame engage}
# starting directory - setwd("E:/Dropbox/FLAME_ColumbiaRiver/Data")
# ===========================================================

# CR_files<-list.files()
# CR_files<-CR_files[which(CR_files!="ColumbiaRiver2016_AllDays")]
# CR_files[1:2]
# 
# days<-CR_files[1]
# for (days in CR_files){
#     FLAMEsubdirectory<-(days)
#     setwd(paste(getwd(), FLAMEsubdirectory, sep="/"))
    
# ===========================================================
# End Open CR Loop Code
# ===========================================================

# read in a metadata file 
# indicates the date (YYYY-MM-DD !!!), site, instruments, flame intervals, and water samples
MetaPath <- list.files(path = paste(getwd(), "/meta", sep=""))
meta<-do.call("rbind", lapply(paste("meta/", MetaPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE)) 
Date<-as.Date(meta$Date[1], format="%Y-%m-%d")
Site<-as.character(meta$Site[1])

# grab samples and atm data if present
if (meta$number_of_samples[1]!=0){
SamplesPath <- list.files(path = paste(getwd(), "/samples", sep=""))
Samples<-do.call("rbind", lapply(paste("samples/", SamplesPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE))}

if (!is.null(meta$number_of_atm_samples[1])){
  if (meta$number_of_atm_samples[1]!=0){
  AtmPath <- list.files(path = paste(getwd(), "/atm", sep=""))
  Atm<-do.call("rbind", lapply(paste("atm/", AtmPath, sep=""), read.table, sep="," ,skip=0,  header = TRUE, fill=TRUE))}}

# Set correct basemap
if(Site=="Peter" | Site=="Paul" | Site=="Tuesday" |Site=="TuesdayLake" | Site=="PeterLake" |Site=="PaulLake"){
  base<-UNDERC_Base
  } else if(Site=="LakeMendota"){
  base<-Mendota_Base
  } else if(Site=="LakeMonona"){
    base<-Monona_Base
  } else if(Site=="Pool8"){
  base<-Pool8_Base
  } else {
  base<-lakes_Base
  }

# Set correct Tau Conversion

if (as.numeric(format(as.Date(meta$Date[1]), "%Y")) == 2015){
  taus<-taus2015.01}
if (as.numeric(format(as.Date(meta$Date[1]), "%Y")) == 2016){
  if (meta$FLAME_Unit[1]=="2016.01"){
    taus<-taus2016.01}
  if (meta$FLAME_Unit[1]=="2016.02"){
    taus<-taus2016.02}
  if (meta$FLAME_Unit[1]=="2016.03"){
    taus<-taus2016.03}
}

# do all 'dem bad things to the FLAMe data
flame_engage_all_maps(meta)

# ===========================================================
# Start close UMR loop code - see above
# ===========================================================

# print(FLAMEsubdirectory)
# setwd("..")
# }
  
# ===========================================================
# End close loop code
# ===========================================================
  
