# =======================================================
# Apply Tau compensations
# =======================================================
tau_correct=function(instrument_table){
require(caTools)
# Using Manual Fit Taus
par(mfrow=c(1,1))

#Temp
X<-instrument_table$TempC
k=3
tau=6
hydro=4
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$Temp_tau<-Tau_X
plot(Tau_X, col="blue", type="l")
points(X, col="black", type="l")

#SpCond
X<-instrument_table$SpCondμScm
k=3
tau=6
hydro=3
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$SpCond_tau<-Tau_X

plot(Tau_X, type="l", col="blue")
points(X, col="black", type="l")

#pH
X<-instrument_table$pH
k=3
tau=6
hydro=2.75
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$pH_tau<-Tau_X

plot(Tau_X, xlim=c(20000, 25000), ylim=c(4.5, 4.65), type="l", col="blue")
points(X, col="black", type="l")

#fDOM
X<-instrument_table$fDOMQSU
k=3
tau=6
hydro=5.5
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$fDOM_QSU_tau<-Tau_X

plot(Tau_X, xlim=c(20000, 25000), ylim=c(40,70), type="l", col="blue")
points(X, col="black", type="l")

#DO
X<-instrument_table$ODOsat
k=3
tau=12
hydro=4.25
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$ODO_tau<-Tau_X

plot(Tau_X, xlim=c(20000, 25000), type="l", col="blue")
points(X, col="black", type="l")

#ChlA
X<-instrument_table$ChlAμgL
k=3
tau=6
hydro=11.2
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$ChlA_tau<-Tau_X

plot(Tau_X, xlim=c(20000, 25000), ylim=c(0, 100), type="l", col="blue")
points(X, col="black", type="l")

#Turb
X<-instrument_table$TurbFNU
k=3
tau=9
hydro=8.25
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$Turb_tau<-Tau_X

plot(Tau_X,  ylim=c(0, 10), type="l", col="blue")
points(X, col="black", type="l")

#CO2
if(is.na(meta$LGR_Unit[1])==FALSE){

X<-instrument_table$XCO2Dppm
k=3
tau=24
hydro=22
lag<-c(X[(1+hydro):(length(X))], rep(NA,times=hydro) )
Diff<-c(diff(X/2, lag=2)[(1+hydro):(length(X))], rep(NA,times=hydro))
Diff_tau_mean<-runmean(tau*Diff, k=k, alg=c("C"), endrule=c("keep"),align = c("center"))
Tau_X<-rep(NA, times=length(X))

for (i in 1:(length(X)-hydro)){
  Tau_X[i]<-(lag[i]+Diff_tau_mean[i])
}
instrument_table$CO2_tau<-Tau_X

plot(Tau_X, type="l", col="blue")
points(X, col="black", type="l")
}
return(instrument_table)
}