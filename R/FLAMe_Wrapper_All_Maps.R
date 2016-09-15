# ===============================
# Flame Wrapper function for handling FLAMe data
# Function combines, cleans, georeferences, and outputs sensor data
# Updated in 2016, Luke Loken
# Initially saved to github Sept 2016
# ===============================

flame_engage_all_maps=function(meta){

#Read and merge all instrument data
mergeddata=read_instruments(meta)

# Cut data based on Flame_On and Flame_Off
cutdata<-flame_cut(mergeddata, meta)

# grab atmospheric CO2 and CH4 data for "Atm_Samples"
if (!is.null(meta$number_of_atm_samples[1])){
  if (meta$number_of_atm_samples[1]!=0){
    atmdata<-extract_flame(mergeddata, Atm, AtmPath)
    # Save to data folder
    write.table(atmdata,  file = as.character(paste(Site, Date, "_AtmSamples", ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  }
}
# Remove Times when Peter Turner (PT) was sampling ambient air
# This is only for Mississippi River data
if (length(grep("PT",meta$Crew[1]))>0){
  cutdata<-PeterTurnerCorrection(cutdata, zerotimes)}

#Apply hydraulic and tau corrections. Different hydro/tau for 2014/2015
Year<-as.numeric(format(as.Date(meta$Date[1]), "%Y"))
if (Year==2014){
  geodata=tau_correct(cutdata)
  geodata=convert_gases(geodata, meta$Elevation[1])}
if (Year>=2015){
  geodata=tau_correct_2015(cutdata)
  geodata=convert_gases_2015(geodata, meta$Elevation[1])}
#Convert CO2 and CH4 to uM and %Sat units
#numeric value (meters) computed from GpsFull$altitude in read instruments script. 

#Save raw data to data folder and archive
write.table(geodata, file = as.character(paste(Site, Date, "raw.csv", sep="")), col.names=TRUE,row.names=F, sep=",")

# Clean data. Remove outliers and errors from sensor/tau outputs
# Uses SensorQC and 'ruletable'
geodata2<-sensorclean(geodata, ruletable)

#Save cleaned data to data folder and archive
write.table(geodata2, file = as.character(paste(Site, Date, "cleaned.csv", sep="")), col.names=TRUE,row.names=F, sep=",")
Syncfolder<-unlist(strsplit(getwd(), split="Dropbox"))[1]
write.table(geodata2, file = as.character(paste(Syncfolder,"Dropbox/FLAME_DataArchive/csvs/", Site, Date, "cleaned.csv", sep="")), col.names=TRUE,row.names=F, sep=",")

#Extract Flame data from water chemistry collection times.
sampledata=extract_flame(geodata2,Samples, SamplesPath)
if (length(sampledata)>0){
  # Save to data folder
  write.table(sampledata,  file = as.character(paste(Site, Date, "_Samples", ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  # Save to archive folder
  write.table(sampledata, file = as.character(paste(Syncfolder,"Dropbox/Flame_DataArchive/waterchemsamples/", Site, Date, "_Samples", ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
}

# Convert dataframe to geospatial object and export shapefile. 
# 'shapefile' has lat/lon in decimal degrees
# Save to data folder
shapefile1<-write_shapefile(geodata, paste(Site, Date, "raw",  sep=""), getwd(), Site, Date, plot=FALSE)
shapefile2<-write_shapefile(geodata2, paste(Site, Date, "cleaned",  sep=""), getwd(), Site, Date, plot=TRUE)
# Save to archive folder
shapefile3<-write_shapefile(geodata2, paste(Site, Date, "cleaned",  sep=""), paste(Syncfolder, "Dropbox/Flame_DataArchive", sep=""), Site, Date, plot=FALSE)

#Plot Heat maps of all parameters in '/maps' subfolders.
#Use basemap provided and plot timeseries below (maps)
Heat_Maps(shapefile2, getwd(), Site, Date)

# No need to plot maps2
# #Use googlemap and plot timeseries below (maps2)
# Google_Heat_Maps(shapefile2, getwd(), Site, Date)

#Use googlemap and make colorbar (maps3)
Google_Heat_Maps_ColorBar(shapefile2, getwd(), Site, Date)

#build semivariance model, and krig all parameters
# FlameKrige(shapefile, subset, )

}
#################END-END-END!########################################################################################