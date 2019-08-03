###################################################
# Araguaia Valley-Croplands x GPP (EVI)           #
#                                                 #
# Eduardo Queiroz Marques-IPAM Canarana-MT/Brazil #
# E-mail: eduardobio2009@gmail.com                #
# 2016 Octuber                                    #
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
setwd("~/Documents/public-ipam/data_geo/Base_raster/Matopiba_Latlong/Spera_tang_latlong")
spera.230<-stack(list.files(pattern='.tif'))
#setwd("~/Documents/public-ipam/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI")
setwd("/home/eduardo/evi_araguaia")
a<-list.files(pattern='.tif')
mos<-stack(a[1:344])
mos.2<-crop(mos,spera.230) #para a Tanguro
spera<-resample(spera.230,mos.2,method='ngb')
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Limite Araguaia/")
limite=readOGR(".","Arag_valley_limit_wgs")
mos.3<-crop(mos,limite)
mos.4<-mask(mos.3,limite)
plot(mos.4[[1:4]])
mos.factor<-mos.4*0.0001;plot(mos.factor[[1:9]])
#==================================================================
#Spera croplands data
setwd("/mnt/data/dados_publicos/Documents/data_geo/Base_raster/Matopiba_Latlong/Spera_latlong/")
sp1=raster("gy2001.tif")
sp2=raster("gy2002.tif")
sp3=raster("gy2003.tif")
sp4=raster("gy2004.tif")
sp5=raster("gy2005.tif")
sp6=raster("gy2006.tif")
sp7=raster("gy2007.tif")
sp8=raster("gy2008.tif")
sp9=raster("gy2009.tif")
sp10=raster("gy2010.tif")
sp11=raster("gy2011.tif")
sp12=raster("gy2012.tif")
sp13=raster("gy2013.tif")
sp15=raster("GY2015.tif")

#Change the extent
sp15b=resample(sp15,sp1, method='ngb')

#Make a stack of all spera datas
spera = raster::stack(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp15b) 


spera <- crop(spera, limite)
spera <- mask(spera, limite)

tab<-c(0,1,2,4,5,6,11,13,99,0,1,2,3,4,5,6,7,8);tab<-matrix(tab,9,2);tab
spera2<-reclassify(spera[[1]],tab)
spera3<-projectRaster(spera2,mos.factor,method="ngb")
#==================================================================
# time
mos.tm <-as.Date(substr(names(mos.factor),10,19),"%Y.%m.%d")
  
mos.factor <- setZ(mos.factor, mos.tm, 'data')


evi.1<-subset(mos.factor, which(getZ(mos.factor)>as.Date("2000-09-01")&
                getZ(mos.factor)<as.Date("2001-08-30")))

evi.spera1=stack(spera3,evi.1)
names(evi.spera2001)

#Class (1=Soybean, 2=Corn, 5=Soybean and corn, 4=Soybean and cotton)
evi.max=max(evi.1);names(evi.max)="maximo"
evi.mean=mean(evi.1);names(evi.mean)="media"

res=zonal(stack(evi.max,evi.mean),spera3,fun="mean")
res

setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Municipios_Araguaia_Valley/")
limit_mun=readOGR(".","Mun_arag_valley_wgs")

## extrair valores para cada municipio
#Class (1=Soybean, 2=Corn, 5=Soybean and corn, 4=Soybean and cotton)
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
resf$ano=2001
head(resf)
#evi.mun$cultura=cll[1]

