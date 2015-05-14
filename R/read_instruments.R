##########Read FLAMe instrument data, construct a single file####################
#################################################################################
read_instruments=function(Site, Date){
require(stringr)
LaserPath <- list.files(path = paste(getwd(), "/laser", sep=""))
#Bind all files in above path using read.table - Need to specify skip, header, etc. 
if (length(LaserPath)>0){
LaserFull<-do.call("rbind", lapply(paste("laser/", LaserPath, sep=""), read.table, sep="," ,skip=1,  header = TRUE, fill=TRUE)) 
LaserFull$Time_raw=as.POSIXct(LaserFull$Time, tz=as.character(meta$LGR_Timezone[1]), format="%m/%d/%y %H:%M:%S")
LaserFull=LaserFull[!duplicated(LaserFull[,"Time_raw"]),]
#This day the LGR was an hour off of UTC
LaserFull$Time<-(LaserFull$Time_raw+meta$LGR_Time_Offset[1])
if (meta$LGR_Unit[1]=="Ankur"){
LaserFull$X.CO2_DRY._ppm<-LaserFull$X.CO2._ppm
LaserFull$X.CH4_DRY._ppm<-LaserFull$X.CH4._ppm
}
laservars <- c("Time", "X.CO2_DRY._ppm", "X.CH4_DRY._ppm")
LaserFull <- LaserFull[laservars]
}
####################################################################################
#read in all of the SUNA converted raw data files using 'suna_datetime' script
NitroPath <- list.files(path = paste(getwd(), "/nitro", sep=""))
#Bind all files in above path using read.table - Need to specify skip, header, etc. 
if (length(NitroPath)>0){
NitroFull<-do.call("rbind", lapply(paste("nitro/", NitroPath, sep=""), read.table, sep=",", header = TRUE, fill=TRUE)) 
NitroFull<-suna_datetime(NitroFull)
nitrovars <- c("Date_Time", "NITRATE_UM",  "NITRATE_MG",	"ABS_254",	"ABS_350", "T_INT",  "T_SPEC",	"T_LAMP")
NitroFull <- NitroFull[nitrovars]
NitroFull<-subset(NitroFull, NITRATE_UM>0)
}
####################################################################################
#read in all of the YSI instrument data files
YsiPath=list.files(path = paste(getwd(), "/ysi", sep=""))
if (length(YsiPath)>0){
YsiFull<-do.call("rbind", lapply(paste("ysi/",YsiPath,  sep=""), read.table, sep="," ,skip=25,  header = TRUE)) 
YsiFull$Date_Time <- as.POSIXct(paste(YsiFull$Date..MM.DD.YYYY., YsiFull$Time..HH.MM.SS.), format="%m/%d/%Y %H:%M:%S", tz=as.character(meta$YSI_Timezone[1]))
YsiFull$Date_Time<-(YsiFull$Date_Time+meta$YSI_Time_Offset[1])
ysivars <- c("Date_Time", "Temp..C", "SpCond.µS.cm", "Chlorophyll.RFU", "Chlorophyll.µg.L", "BGA.PC.RFU", "BGA.PC.µg.L", "Turbidity.FNU", "fDOM.RFU", "fDOM.QSU", "ODO...sat", "ODO.mg.L", "pH", "Press.psi.a")
YsiFull <- YsiFull[ysivars]
}
######################################################################################
#read in all of the GPS files
GpsPath=list.files(path = paste(getwd(), "/gps", sep=""))
GpsAll<-do.call("rbind", lapply(paste("gps/", GpsPath, sep=""), read.table, sep="," , header = TRUE)) 
GpsAll$ltime=as.POSIXct(GpsAll$ltime, tz=as.character(meta$GPS_Timezone[1]))
GpsAll$ltime<-(GpsAll$ltime+meta$GPS_Time_Offset[1])
######################################################################################
#cut gps to FLAME_ON intervals
i=1
GpsFull<-data.frame() 
for (i in 1:length(meta$Flame_On)){
  interval=as.POSIXct(c(Flame_On[i], Flame_Off[i]),tz="America/Chicago")
  GpsFull<-rbind(GpsFull, subset(GpsAll,GpsAll$ltime>=interval[1] & GpsAll$ltime<=interval[2]))
}
gpsvars <- c("ltime", "Latitude", "Longitude","altitude" )
GpsFull <- GpsFull[gpsvars]
#str(GpsFull)
#plot(GpsFull$Longitude,GpsFull$Latitude, col="red", cex=.5)
#################################################################################
#merge all of the instrument datasets together
#################################################################################
if (length(LaserPath)>0){
  merge.data1=merge(GpsFull, LaserFull,   by.y="Time", by.x=c("ltime"),all.x=TRUE)
}
if (length(LaserPath)==0) {
  merge.data1=GpsFull
}

if (length(YsiPath)>0){
  merge.data2=merge(merge.data1, YsiFull, by.y="Date_Time", by.x="ltime",all.x=TRUE)
}
if (length(YsiPath)==0) {
  merge.data2=merge.data1
}

if (length(NitroPath)>0){
merge.data3=merge(merge.data2, NitroFull, by.y="Date_Time", by.x="ltime",all.x=TRUE)
}
if (length(NitroPath)==0) {
  merge.data3=merge.data2
}

colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "[.]", "")
colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "[_]", "")
colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "[µ]", "u")
colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "Chlorophyll", "ChlA")
colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "Turbidity", "Turb")
colnames(merge.data3)<-str_replace_all(colnames(merge.data3), "DRY", "D")

##write out to a single .csv file###############################################
write.table(merge.data3, file = as.character(paste(Site, Date, ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
return(merge.data3)
}
########################END###########################################################