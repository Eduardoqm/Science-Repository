###################################################
# Araguaia Valley-Croplands x EVI                 #
#                                                 #
# Eduardo Queiroz Marques-IPAM Canarana-MT/Brazil #
# E-mail: eduardobio2009@gmail.com                #
# 2016 November                                   #
###################################################

#About this Script

#Packages
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
library(MODIS)
library(sp)
library(gdalUtils)
library(breakpoint)
library(plotrix)
library(circular)

#limpando workspace
rm(list=ls(all=TRUE))
setwd("/home/eduardo/evi_araguaia")
a<-list.files(pattern='.tif')
mos<-stack(a[1:344])
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Limite Araguaia/")
limite=readOGR(".","Arag_valley_limit_wgs")
mos.3<-crop(mos,limite)
mos.factor<-mos.3*0.0001;plot(mos.factor[[1:9]])
#===================================================================================================
##Salve EVI data
setwd("/home/eduardo/evi_araguaia2/")
writeRaster(mos.factor, filename="EVI_arag", options="INTERLEAVE=BAND", overwrite=TRUE)
#===================================================================================================
#Spera croplands data
setwd("/mnt/data/dados_publicos/Documents/data_geo/Base_raster/Matopiba_Latlong/Spera_latlong/")
xx<-list.files(,pattern=".tif");xx
spera<-stack(xx[1:13])

sp15=raster("GY2015.tif")

#Change the extent
sp15b=resample(sp15,spera[[1]], method='ngb')

#Make a stack of all spera datas
spera = stack(spera, sp15b) 


spera <- crop(spera, limite)
spera <- mask(spera, limite)

tab<-c(0,1,2,4,5,6,11,13,99,0,1,2,3,4,5,6,7,8);tab<-matrix(tab,9,2);tab
spera2<-reclassify(spera,tab) 
spera3<-projectRaster(spera2,mos.factor,method="ngb")
#===================================================================================================
##Saving Spera data
setwd("/home/eduardo/evi_araguaia2/")
writeRaster(spera3, filename="Spera", options="INTERLEAVE=BAND", overwrite=TRUE)
#===================================================================================================
#NEW STEP
#===================================================================================================
##Loading data
setwd("/home/eduardo/evi_araguaia2/")
mos.factor=stack("EVI_arag.grd")
spera3=stack("Spera.grd")
#===================================================================================================
#creating time series
mos.tm <-as.Date(substr(names(mos.factor),10,19),"%Y.%m.%d")

mos.factor <- setZ(mos.factor, mos.tm, 'data')


evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2012-09-01")&
                                  getZ(mos.factor)<as.Date("2013-08-30")))
#--------------------------------------------
#--------------------------------------------
#selecting land uses

##Class (1=Soybean, 2=Corn, 3=Cotton, 4=Soybean and cotton, 5=Soybean and corn)

#Soybean
fun <- function(x) { x[x[[1]]!=1] <- 0; return(x) }
evi.soy.2001 <- calc(evi.spera2001, fun);evi.soy.2001 = evi.soy.2001[[-1]]
plot(evi.soy.2001)
evi.soy.2002 <- calc(evi.spera2002, fun);evi.soy.2002 = evi.soy.2002[[-1]]
plot(evi.soy.2002)
evi.soy.2003 <- calc(evi.spera2003, fun);evi.soy.2003 = evi.soy.2003[[-1]]
plot(evi.soy.2003)
evi.soy.2004 <- calc(evi.spera2004, fun);evi.soy.2004 = evi.soy.2004[[-1]]
plot(evi.soy.2004)
evi.soy.2005 <- calc(evi.spera2005, fun);evi.soy.2005 = evi.soy.2005[[-1]]
plot(evi.soy.2005)
evi.soy.2006 <- calc(evi.spera2006, fun);evi.soy.2006 = evi.soy.2006[[-1]]
plot(evi.soy.2006)
evi.soy.2007 <- calc(evi.spera2007, fun);evi.soy.2007 = evi.soy.2007[[-1]]
plot(evi.soy.2007)
evi.soy.2008 <- calc(evi.spera2008, fun);evi.soy.2008 = evi.soy.2008[[-1]]
plot(evi.soy.2008)
evi.soy.2009 <- calc(evi.spera2009, fun);evi.soy.2009 = evi.soy.2009[[-1]]
plot(evi.soy.2009)
evi.soy.2010 <- calc(evi.spera2010, fun);evi.soy.2010 = evi.soy.2010[[-1]]
plot(evi.soy.2010)
evi.soy.2011 <- calc(evi.spera2011, fun);evi.soy.2011 = evi.soy.2011[[-1]]
plot(evi.soy.2011)
evi.soy.2012 <- calc(evi.spera2012, fun);evi.soy.2012 = evi.soy.2012[[-1]]
plot(evi.soy.2012)
evi.soy.2013 <- calc(evi.spera2013, fun);evi.soy.2013 = evi.soy.2013[[-1]]
plot(evi.soy.2013)
evi.soy.2015 <- calc(evi.spera2015, fun);evi.soy.2015 = evi.soy.2015[[-1]]
plot(evi.soy.2015)

#Soybean and corn
fun <- function(x) { x[x[[1]]!=5] <- 0; return(x) }
evi.soyc.2001 <- calc(evi.spera2001, fun);evi.soyc.2001 = evi.soyc.2001[[-1]]
plot(evi.soyc.2001)
evi.soyc.2002 <- calc(evi.spera2002, fun);evi.soyc.2002 = evi.soyc.2002[[-1]]
plot(evi.soyc.2002)
evi.soyc.2003 <- calc(evi.spera2003, fun);evi.soyc.2003 = evi.soyc.2003[[-1]]
plot(evi.soyc.2003)
evi.soyc.2004 <- calc(evi.spera2004, fun);evi.soyc.2004 = evi.soyc.2004[[-1]]
plot(evi.soyc.2004)
evi.soyc.2005 <- calc(evi.spera2005, fun);evi.soyc.2005 = evi.soyc.2005[[-1]]
plot(evi.soyc.2005)
evi.soyc.2006 <- calc(evi.spera2006, fun);evi.soyc.2006 = evi.soyc.2006[[-1]]
plot(evi.soyc.2006)
evi.soyc.2007 <- calc(evi.spera2007, fun);evi.soyc.2007 = evi.soyc.2007[[-1]]
plot(evi.soyc.2007)
evi.soyc.2008 <- calc(evi.spera2008, fun);evi.soyc.2008 = evi.soyc.2008[[-1]]
plot(evi.soyc.2008)
evi.soyc.2009 <- calc(evi.spera2009, fun);evi.soyc.2009 = evi.soyc.2009[[-1]]
plot(evi.soyc.2009)
evi.soyc.2010 <- calc(evi.spera2010, fun);evi.soyc.2010 = evi.soyc.2010[[-1]]
plot(evi.soyc.2010)
evi.soyc.2011 <- calc(evi.spera2011, fun);evi.soyc.2011 = evi.soyc.2011[[-1]]
plot(evi.soyc.2011)
evi.soyc.2012 <- calc(evi.spera2012, fun);evi.soyc.2012 = evi.soyc.2012[[-1]]
plot(evi.soyc.2012)
evi.soyc.2013 <- calc(evi.spera2013, fun);evi.soyc.2013 = evi.soyc.2013[[-1]]
plot(evi.soyc.2013)
evi.soyc.2015 <- calc(evi.spera2015, fun);evi.soyc.2015 = evi.soyc.2015[[-1]]
plot(evi.soyc.2015)

#Corn
fun <- function(x) { x[x[[1]]!=2] <- 0; return(x) }
evi.corn.2001 <- calc(evi.spera2001, fun);evi.corn.2001 = evi.corn.2001[[-1]]
plot(evi.corn.2001)
evi.corn.2002 <- calc(evi.spera2002, fun);evi.corn.2002 = evi.corn.2002[[-1]]
plot(evi.corn.2002)
evi.corn.2003 <- calc(evi.spera2003, fun);evi.corn.2003 = evi.corn.2003[[-1]]
plot(evi.corn.2003)
evi.corn.2004 <- calc(evi.spera2004, fun);evi.corn.2004 = evi.corn.2004[[-1]]
plot(evi.corn.2004)
evi.corn.2005 <- calc(evi.spera2005, fun);evi.corn.2005 = evi.corn.2005[[-1]]
plot(evi.corn.2005)
evi.corn.2006 <- calc(evi.spera2006, fun);evi.corn.2006 = evi.corn.2006[[-1]]
plot(evi.corn.2006)
evi.corn.2007 <- calc(evi.spera2007, fun);evi.corn.2007 = evi.corn.2007[[-1]]
plot(evi.corn.2007)
evi.corn.2008 <- calc(evi.spera2008, fun);evi.corn.2008 = evi.corn.2008[[-1]]
plot(evi.corn.2008)
evi.corn.2009 <- calc(evi.spera2009, fun);evi.corn.2009 = evi.corn.2009[[-1]]
plot(evi.corn.2009)
evi.corn.2010 <- calc(evi.spera2010, fun);evi.corn.2010 = evi.corn.2010[[-1]]
plot(evi.corn.2010)
evi.corn.2011 <- calc(evi.spera2011, fun);evi.corn.2011 = evi.corn.2011[[-1]]
plot(evi.corn.2011)
evi.corn.2012 <- calc(evi.spera2012, fun);evi.corn.2012 = evi.corn.2012[[-1]]
plot(evi.corn.2012)
evi.corn.2013 <- calc(evi.spera2013, fun);evi.corn.2013 = evi.corn.2013[[-1]]
plot(evi.corn.2013)
evi.corn.2015 <- calc(evi.spera2015, fun);evi.corn.2015 = evi.corn.2015[[-1]]
plot(evi.corn.2015)

#Soybean and cotton
fun <- function(x) { x[x[[1]]!=4] <- 0; return(x) }
evi.soycot.2001 <- calc(evi.spera2001, fun);evi.soycot.2001 = evi.soycot.2001[[-1]]
plot(evi.soycot.2001)
evi.soycot.2002 <- calc(evi.spera2002, fun);evi.soycot.2002 = evi.soycot.2002[[-1]]
plot(evi.soycot.2002)
evi.soycot.2003 <- calc(evi.spera2003, fun);evi.soycot.2003 = evi.soycot.2003[[-1]]
plot(evi.soycot.2003)
evi.soycot.2004 <- calc(evi.spera2004, fun);evi.soycot.2004 = evi.soycot.2004[[-1]]
plot(evi.soycot.2004)
evi.soycot.2005 <- calc(evi.spera2005, fun);evi.soycot.2005 = evi.soycot.2005[[-1]]
plot(evi.soycot.2005)
evi.soycot.2006 <- calc(evi.spera2006, fun);evi.soycot.2006 = evi.soycot.2006[[-1]]
plot(evi.soycot.2006)
evi.soycot.2007 <- calc(evi.spera2007, fun);evi.soycot.2007 = evi.soycot.2007[[-1]]
plot(evi.soycot.2007)
evi.soycot.2008 <- calc(evi.spera2008, fun);evi.soycot.2008 = evi.soycot.2008[[-1]]
plot(evi.soycot.2008)
evi.soycot.2009 <- calc(evi.spera2009, fun);evi.soycot.2009 = evi.soycot.2009[[-1]]
plot(evi.soycot.2009)
evi.soycot.2010 <- calc(evi.spera2010, fun);evi.soycot.2010 = evi.soycot.2010[[-1]]
plot(evi.soycot.2010)
evi.soycot.2011 <- calc(evi.spera2011, fun);evi.soycot.2011 = evi.soycot.2011[[-1]]
plot(evi.soycot.2011)
evi.soycot.2012 <- calc(evi.spera2012, fun);evi.soycot.2012 = evi.soycot.2012[[-1]]
plot(evi.soycot.2012)
evi.soycot.2013 <- calc(evi.spera2013, fun);evi.soycot.2013 = evi.soycot.2013[[-1]]
plot(evi.soycot.2013)
evi.soycot.2015 <- calc(evi.spera2015, fun);evi.soycot.2015 = evi.soycot.2015[[-1]]
plot(evi.soycot.2015)

#===================================================================================================
#EVI values (max, mean, median)
#Está para 2013 porque está em teste

#Soybean
evi.soy.max=max(evi.soy.2013);names(evi.max)="maximo"
evi.soy.mean=mean(evi.soy.2013);names(evi.mean)="media"
evi.soy.median=calc(evi.soy.2013,fun=median);names(evi.median)="median"

#Soy and Corn
evi.soyc.max=max(evi.soyc.2013);names(evi.max)="maximo"
evi.soyc.mean=mean(evi.soyc.2013);names(evi.mean)="media"
evi.soyc.median=calc(evi.soyc.2013,fun=median);names(evi.median)="median"

#Corn
evi.corn.max=max(evi.corn.2013);names(evi.max)="maximo"
evi.corn.mean=mean(evi.corn.2013);names(evi.mean)="media"
evi.corn.mediana=median(evi.corn.2013,na.rm=TRUE);names(evi.mediana)="mediana"

#Soy and Cotton
evi.soycot.max=max(evi.soycot.2013);names(evi.max)="maximo"
evi.soycot.mean=mean(evi.soycot.2013);names(evi.mean)="media"
evi.soycot.median=calc(evi.soycot.2013,fun=median);names(evi.median)="median"












## extrair valores para cada municipio
#Class (1=Soybean, 2=Corn, 3=Cotton, 4=Soybean and cotton, 5=Soybean and corn)
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Municipios_Araguaia_Valley/")
limit_mun=readOGR(".","Mun_arag_valley_wgs")
mun_arag=rasterize(limit_mun)

cll=c(1,2,4,5)

res1=list()

for(i in 1:4){
  cat("\r",i)  
  funf <- function(x) { x[x[[1]]!=cll[i]] <- NA;return(x)}
  
  evi.ff=calc(stack(spera3,evi.max,evi.mean),fun=funf)
  
  limit_mun@data$evi.mean <- extract(evi.ff[[3]], limit_mun, fun=mean,na.rm=TRUE)
  
  df0=limit_mun@data
  df0$cultura=cll[i]
  res1[[i]]=df0
}

resf=do.call('rbind',res1)
resf$ano=2013
head(resf)
#evi.mun$cultura=cll[1]