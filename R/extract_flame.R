
# =======================================================
# Extract Flame Sample Times
# Merge with water chemistry samples
# =======================================================


#sampledata=extract_flame(geodata, meta)

extract_flame=function(table, samples){
  library(caTools)
  samplevars<-c ("Sample.Number", "Sample.Time", "Sample.Notes")
  samples<-samples[samplevars]
  samples<-samples[is.na(samples$Sample.Number)==FALSE,]
  if (length(samples$Sample.Time)>0){
  samples$DateTime<-as.POSIXct(paste(Date,samples$Sample.Time, sep=" "), format="%Y-%m-%d %H:%M:%S", tz=as.character(meta$GPS_Timezone[1]))
    
  table$ltime<-as.POSIXct(round(table$ltime, "mins"))
 
  table2<-as.data.frame(aggregate(table, by=list(table$ltime), FUN=mean, na.rm=TRUE))
  tablevars<-names(table)
  table2<-table2[tablevars]
  
  Flame_times<-merge(samples, table2, by.x="DateTime", by.y="ltime" )
  
  write.table(Flame_times, file = as.character(paste("samples/FlameSites", Site, Date, ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
  
  return(Flame_times)
  }
}