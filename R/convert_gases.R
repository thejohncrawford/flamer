# =====================================================
# CONVERSION CODE - Use getkH for CH4 and KH_Plummber for CO2
# Use getSaturation for both gases
# =====================================================
convert_gases=function(instrument_table, Elevation){
  if(is.na(meta$LGR_Unit[1])==FALSE){
  
  #function to calculate equilibrium concentration of CO2 in water due to changes in water temperature
  #Plummer and Busenberg 1982; Geochimica et Cosmochimica Acta
  Kh_Plummer <- function(Temperature){
    kh_Plummer=10^(108.3865 + 0.01985076*Temperature - 6919.53*(Temperature^-1) -40.45154*log10(Temperature)+669365*(Temperature^-2))
    return(kh_Plummer)
  }      
  #Henry's Law constants from http://www.henrys-law.org/henry.pdf
  #Temperature correction using van't Hoff equation , temperature is in (Kelvin)
  #LakeKh is in ("Kh, cp") = (mol/L*Atm) at STP
  getKh <- function(temperature, gas){
    Kh  <-  data.frame("O2"=c(1.3*10^-3, 1700))
    Kh <- cbind(Kh,"H2"=c(7.8*10^-4,500))
    Kh <- cbind(Kh,"CO2"= c(3.4*10^-2, 2400 ))
    Kh <- cbind(Kh,"N2"=c(6.1*10^-4, 1300))
    Kh <- cbind(Kh,"He"=c(3.7*10^-4, 230))
    Kh <- cbind(Kh,"Ne"=c(4.5*10^-4,490))
    Kh <- cbind(Kh,"Ar"=c(1.4*10^-3, 1300))
    Kh <- cbind(Kh,"CO"=c(9.5*10^-4,1300))
    Kh <- cbind(Kh, "O3"=c(1.2*10^-2, 2300))
    Kh <- cbind(Kh, "N2O"=c(2.5*10^-2, 2600))
    Kh <- cbind(Kh, "SF6"=c(2.4*10^-4, 2400))
    Kh <- cbind(Kh, "CH4"=c(1.4*10^-3, 1700))
    Kh <- cbind(Kh, "C3H8"=c(1.4*10^-3, 2700))
    Kh <- cbind(Kh, "NH3"=c(5.6*10^1, 4200))
    
    if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
    if (!any(names(Kh)==gas)){stop(paste(gas,'not found in list of coded gases'))}
    
    Khprime <-  unlist(Kh[gas])[1]
    C  <-  unlist(Kh[gas])[2]
    
    LakeKh= as.numeric(Khprime*exp(C*((1/temperature)-(1/298))))
  }
  getSaturation <- function(LakeKh, AtmP, gas){
    Atmosphere  <-  data.frame("O2"=209460)
    Atmosphere <- cbind(Atmosphere,"H2"=0.55)
    Atmosphere <- cbind(Atmosphere, "N2"=780840)
    Atmosphere <- cbind(Atmosphere, "Ar"=9340)
    Atmosphere <-cbind(Atmosphere, "CO2"=400)
    Atmosphere <-cbind(Atmosphere, "He"=5.24)
    Atmosphere <-cbind(Atmosphere, "Ne"=18.18)
    Atmosphere <- cbind(Atmosphere, "CH4"= 1.91)
    Atmosphere <- cbind(Atmosphere, "O3"=0.07)#potential max concentration
    Atmosphere <-cbind(Atmosphere, "N2O"= 0.325)
    Atmosphere <-cbind(Atmosphere, "CO"=0.1)
    Atmosphere <-cbind(Atmosphere, "NH3"=NA)
    
    if (!is.character(gas)){stop(paste('gas must be a character. was given as',gas))}
    if (!any(names(Atmosphere)==gas)){stop(paste(gas,'not found in list of coded gases'))}
    
    AtmosphereConc <-  unlist(Atmosphere[gas])[1]
    
    EquilSaturation=AtmosphereConc*LakeKh/AtmP #umol/L, mmol/m3
  }
Pressure=(1-(.0000225577*Elevation))^5.25588# atmospheres
#convert methane units 
CH4kh=getKh(instrument_table$Temp_tau+273.15,"CH4")
CH4getsat=as.numeric(getSaturation(CH4kh, Pressure, "CH4"))
CH4uM=instrument_table$XCH4Dppm*CH4kh/Pressure
CH4Sat<-as.numeric(CH4uM/CH4getsat*100)
#convert CO2 units
CO2kh=Kh_Plummer(instrument_table$Temp_tau+273.15)
CO2getsat=as.numeric(getSaturation(CO2kh, Pressure, "CO2"))
CO2uM=instrument_table$XCO2Dppm*CO2kh/Pressure
CO2Sat<-as.numeric(CO2uM/CO2getsat*100)
CO2uM_tau=instrument_table$CO2_tau*CO2kh/Pressure
CO2Sat_tau<-as.numeric(CO2uM_tau/CO2getsat*100)
#add the converted values to the table
instrument_table$CH4uM<-as.numeric(CH4uM)
instrument_table$CH4Sat<-as.numeric(CH4Sat)
instrument_table$CO2uM<-as.numeric(CO2uM)
instrument_table$CO2Sat<-as.numeric(CO2Sat)
instrument_table$CO2uM_tau<-as.numeric(CO2uM_tau)
instrument_table$CO2Sat_tau<-as.numeric(CO2Sat_tau)
#
#instrument_table[is.na(instrument_table)]<-""
}
return(instrument_table)
}