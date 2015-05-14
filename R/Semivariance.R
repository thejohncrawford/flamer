
library(maptools)
library(rgdal)
library(sp)
library(gstat)
library(raster)
library(geoR)
library(plotKML)
library(automap)
library(ggplot2)
library(colorRamps)
library(squash)
library(caTools)
library(kriging)
library(RgoogleMaps)


# Resource: http://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# projections http://spatialreference.org/ref/epsg/4326/

Semivariance=function(shape){
  
  shape_UTM<-spTransform(shape, CRS("+proj=utm +zone=16 ellps=WGS84"))
  
  i=5
  #cutoff= #need to set to be 1/3 to 1/2 the spatial width
  #width= #need to set to be 1/3 to 1/2 the spatial width
#  for (i in 1:length(shape_UTM@data)){

    name<-names(shape_UTM@data)[i]
    b<-shape[is.na(shape_UTM@data[,i])==FALSE,]
#Variogram model 1 - Autofit
    variogram1 = autofitVariogram(b@data[,i]~1,b, model="Sph")
    plot(variogram1)
    summary(variogram1)

#Variogram model 2 - set lags
variogram2<-variogram(b@data[,i]~1,b, width=10, cutoff=3000)
summary(variogram2)
plot(variogram2)
model<-fit.variogram(variogram2, vgm(1, "Sph", 1000, 1), fit.sills=TRUE, fit.ranges=TRUE )
summary(model)
plot(variogram2, model=model, col="black", xlab="lag distance (m)", cex=0.6)

#}


}

##############################################
#Test code below
##############################################

#Two methods for calcualting semivariograms. AutofitVariogram does a good job, but you cannot adjust the lag parameters and plotting mode, Variogram followed by fit.variogram do the same functions, but can be customized. You must supply an initial model using vgm(psill, model, range, nugget). If these are reasonable estimates it will produce the same results. If these are very bad the end product will not be meaningful. 

variogram2 = autofitVariogram(CH4uM~1,CH4.df, model="Sph")
plot(variogram2)
summary(variogram2)

variogram_CH4<-variogram(CH4uM~1,CH4.df, width=20, cutoff=3000)
summary(variogram_CH4)
plot(variogram_CH4)
model_CH4<-fit.variogram(variogram_CH4, vgm(1, "Sph", 1000, 1), fit.sills=TRUE, fit.ranges=TRUE )
summary(model_CH4)
plot(variogram_CH4, model=model_CH4, col="black", xlab="lag distance (m)", cex=0.6)

#Cond models

Cond.df<-Data.df[is.na(Data.df$SpConduScm)==FALSE & Data.df$SpConduScm<110,]

variogram3 = autofitVariogram(SpConduScm~1,Cond.df, model="Sph")
plot(variogram3)
summary(variogram3)

variogram_Cond<-variogram(SpConduScm~1,Cond.df, width=25, cutoff=3000)
summary(variogram_Cond)
plot(variogram_Cond)
model_Cond<-fit.variogram(variogram_Cond, vgm(20, "Sph", 2000, 3), fit.sills=TRUE, fit.ranges=TRUE )
summary(model_Cond)
plot(variogram_Cond, model=model_Cond)

#Simple semivariogram plots

png("CH4_Semivariance_ALCreek_July.png",res=200, width=4,height=2.5, units="in")

par(mar=c(2,1.5,2,0),bg=NA)
par(mfrow=c(1,1)) 
par( oma = c( .5,1.5,0,0 ) )
par(tck=-.02)
par(mgp=c(0,.6,0))
par(las=0)

plot(variogram_CH4, model=model_CH4, col="black", xlab="lag distance (m)", cex=0.6)

dev.off()

png("Cond_Semivariance_ALCreek_July.png",res=200, width=4,height=2.5, units="in")

par(mar=c(2,1.5,2,0),bg=NA)
par(mfrow=c(1,1)) 
par( oma = c( .5,1.5,0,0 ) )
par(tck=-.02)
par(mgp=c(0,.6,0))
par(las=0)

plot(variogram_Cond, model=model_Cond, col="black", xlab="lag distance (m)", cex=0.6)

dev.off()

# ==========================================
# END good code. Everything below is testing
# ==========================================

#krig_test<-autoKrige(CH4uM~1,CH4)

kriging( Data3.project$Longitude, Data3.project$Latittude, Data3.project$CH4uM, model="spherical", lags=10, pixels=100)


#Test google maps
#Need to keep data in lat/lon

B<-100 #Number of breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

Data$Col <- as.numeric(cut(Data$CH4uM,breaks = B))
Data$Color<-colors[Data$Col]
plot(Data, col=Data$Color, pch=16)
plot(Data$CH4uM, col=Data$Color, pch=16)

map1<-GetMap(center=c(lat=46.03, lon=-89.62), size=c(640,640), zoom=13, maptype=c("satellite"), destfile="GoogleMapTest.png")
str(map1)
PlotOnStaticMap(map1)

PlotOnStaticMap(map1, lat=Data$Latitude, lon=Data$Longitude, col=Data$Color, pch=16, FUN=points, destfile="GoogleMapTest3.png", add=T)

map2<-PlotOnStaticMap(map1, lat=Data$Latitude, lon=Data$Longitude, col=Data$Color, pch=16, FUN=points, destfile="GoogleMapTest3.png", add=T)

PlotOnStaticMap(map2)
str(map2)

map3<- gmap(center=c(lat=46.03, lon=-89.62), size=c(640, 940), type="satellite", zoom= 13, filename="Wisconsintest.gmap")
plot(map3)

#maps using maptools

grid1<-GE_SpatialGrid(Data3, maxPixels=600)
plot(Data3)


mymap<-gmap()

shape1<-readShapeSpatial("C:/Users/Luke/Dropbox/FLAME/Data/ArcMaps_Sept4_2014/Vilas Co Ortho Imagery/ortho_1-1_1n_s_wi125_2013_1.shp")
summary(shape1)
proj4string(shape1)=CRS("+proj=utm +zone=16 ellps=WGS84")
plot(shape1, layer="Red")
points(CH4, col=CH4$Color, pch=16)

plotRGB(shape1, r=1, g=2, b=3)


#GeoR test code. computes variogram using Matern model (a mix of other types of models each with different weights) - This code also includes estimates of variance around each parameter based on Bayseian principles. 

vario100 <- variog(s100, max.dist=1)
ini.vals <- expand.grid(seq(0,1,l=5), seq(0,1,l=5))
ols <- variofit(vario100, ini=ini.vals, fix.nug=TRUE, wei="equal")
summary(ols)
wls <- variofit(vario100, ini=ini.vals, fix.nug=TRUE)
summary(wls)
plot(vario100)
lines(wls)
lines(ols, lty=2)



## Not run:
# generating a simulated data-set
ex.data <- grf(70, cov.pars=c(10, .15), cov.model="matern", kappa=2)
#
# defining the grid of prediction locations:
ex.grid <- as.matrix(expand.grid(seq(0,1,l=21), seq(0,1,l=21)))
#
# computing posterior and predictive distributions
# (warning: the next command can be time demanding)
ex.bayes <- krige.bayes(ex.data, loc=ex.grid,
                        model = model.control(cov.m="matern", kappa=2),
                        prior = prior.control(phi.discrete=seq(0, 0.7, l=51),
                                              phi.prior="reciprocal"))
#
# Prior and posterior for the parameter phi
plot(ex.bayes, type="h", tausq.rel = FALSE, col=c("red", "blue"))
#
# Plot histograms with samples from the posterior
par(mfrow=c(3,1))
hist(ex.bayes)
par(mfrow=c(1,1))
# Plotting empirical variograms and some Bayesian estimates:
# Empirical variogram

plot(variog(ex.data, max.dist = 1), ylim=c(0, 15))
# Since ex.data is a simulated data we can plot the line with the "true" model
lines.variomodel(ex.data, lwd=2)
# adding lines with summaries of the posterior of the binned variogram
lines(ex.bayes, summ = mean, lwd=1, lty=2)
lines(ex.bayes, summ = median, lwd=2, lty=2)
# adding line with summary of the posterior of the parameters
lines(ex.bayes, summary = "mode", post = "parameters")
# Plotting again the empirical variogram
plot(variog(ex.data, max.dist=1), ylim=c(0, 15))
# and adding lines with median and quantiles estimates
my.summary <- function(x){quantile(x, prob = c(0.05, 0.5, 0.95))}
lines(ex.bayes, summ = my.summary, ty="l", lty=c(2,1,2), col=1)
# Plotting some prediction results
op <- par(no.readonly = TRUE)
par(mfrow=c(2,2), mar=c(4,4,2.5,0.5), mgp = c(2,1,0))
image(ex.bayes, main="predicted values")
image(ex.bayes, val="variance", main="prediction variance")
image(ex.bayes, val= "simulation", number.col=1,
      main="a simulation from the \npredictive distribution")
image(ex.bayes, val= "simulation", number.col=2,
      main="another simulation from \nthe predictive distribution")
#
par(op)



plot(s100)
## Not run:
# Defining a prediction grid
loci <- expand.grid(seq(0,1,l=21), seq(0,1,l=21))
# predicting by ordinary kriging
kc <- krige.conv(s100, loc=loci,
                 krige=krige.control(cov.pars=c(1, .25)))
# mapping point estimates and variances
par.ori <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(3.5,3.5,1,0), mgp=c(1.5,.5,0))
image(kc, main="kriging estimates")
image(kc, val=sqrt(kc$krige.var), main="kriging std. errors")
# Now setting the output to simulate from the predictive
# (obtaining conditional simulations),
# and to compute quantile and probability estimators
s.out <- output.control(n.predictive = 1000, quant=0.9, thres=2)
set.seed(123)
kc <- krige.conv(s100, loc = loci,
                 krige = krige.control(cov.pars = c(1,.25)),
                 output = s.out)
par(mfrow=c(2,2))
image(kc, val=kc$simul[,1], main="a cond. simul.")
image(kc, val=kc$simul[,1], main="another cond. simul.")
image(kc, val=(1 - kc$prob), main="Map of P(Y > 2)")
image(kc, val=kc$quant, main="Map of y s.t. P(Y < y) = 0.9")
par(par.ori)
## End(Not run)
