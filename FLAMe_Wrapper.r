######################################################################
#wrapper function for handling FLAMe data
#######################################################################################################
#run functions to compile data to single table, correct instrument responses and write to GIS shapefile
#######################################################################################################
flame_engage=function(meta){
#Read and merge all instrument data according to flame on intervals 
instruments=read_instruments(Site, Date)
#Apply hydraulic and tau corrections.
geodata=tau_correct(instruments)
#Convert CO2 and CH4 to uM and %Sat units
#numeric value (meters) computed from GpsFull$altitude in read instruments script. 
geodata=convert_gases(geodata, median(instruments$altitude))
#Extract Flame data from water chemistry collection times.
sampledata=extract_flame(geodata,Samples)
#Convert dataframe to geospatial object and export shapefile. 
#'shapefile' has lat/lon in decimal degrees
shapefile<-write_shapefile(geodata, paste(Site, Date,  sep=""), getwd(), Site, Date)
#Plot Heat maps of all parameters in /maps subfolder.
Heat_Maps(shapefile, getwd(), Site, Date)
#Semivariance(shapefile) #Still working on
}
#################END-END-END!########################################################################################

#Test Test add comments to commit