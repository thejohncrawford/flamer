
# =======================================================
# Extract Flame Sample Times
# Merge with water chemistry samples
# =======================================================


#sampledata=extract_flame(geodata, meta)

extract_flame=function(table, samples, SamplesPath){
  if (meta$number_of_samples[1]!=0){
  library(caTools)
  samplevars<-c ("sample_id", "event_id", "Sample.Number", "Sample.Time", "Sample.Notes")
  samples<-samples[is.na(samples$Sample.Time)==FALSE,]
  samples$DateTime<-as.POSIXct(round(as.POSIXct(paste(Date,samples$Sample.Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz=as.character(meta$GPS_Timezone[1])), "mins"))
    
  table$ltime<-as.POSIXct(round(table$ltime, "mins"))
 
  table2<-as.data.frame(aggregate(table, by=list(table$ltime), FUN=mean, na.rm=TRUE))
  tablevars<-names(table)
  table2<-table2[tablevars]
  
  Flame_times<-merge(samples, table2, by.x="DateTime", by.y="ltime" )
  
  write.table(Flame_times,  file = as.character(paste(Site, Date, "_Samples", ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")

  folder<-unlist(strsplit(getwd(), split="Data"))[1]
  write.table(Flame_times, file = as.character(paste(folder,"Data/WaterChemSamples/", Site, Date, "_Samples", ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  
  return(Flame_times)
  }
}