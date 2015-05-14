#Code to generate datetimes from suna 'converted raw' files
#nitro is a suna converted dataframe
#code addes a new column to existing dataframe called 'DateTime'
#TIME is in decimal hour (e.g., 17.640)
#DATE is the four digit year followed by 'day of the year' (e.g., 2015104)

suna_datetime=function(nitro){
  
library(lubridate)

hour<-c(floor(nitro$TIME))
a<-(nitro$TIME-hour)*60
minute<-floor(a)
second<-round((a-minute)*60)

date<-as.POSIXct(strptime(paste(nitro$DATE,hour, minute, second), format="%Y%j %H%M%S"), tz=as.character(meta$SUNA_Timezone[1]))

nitro$Date_Time<-date

nitro_range <- interval(min(date[is.na(date)==FALSE]), max(date[is.na(date)==FALSE]))
b<-as.POSIXct((c(min(date[is.na(date)==FALSE]), max(date[is.na(date)==FALSE]))), tz="American/Chicago")
nitro_range_CST<-interval(b[1], b[2])
print(list(nitro_range, nitro_range_CST))
return(nitro)
}

# nitro<-read.csv("C:/Users/Luke/Dropbox/FLAME/Data/2015-04-11_Pool8/nitro/D2015101-SUNA0334.csv", header=TRUE)
# 
# suna_datetime(nitro)
