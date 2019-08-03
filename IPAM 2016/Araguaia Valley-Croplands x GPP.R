###################################################
# Araguaia Valley-Croplands x GPP (EVI)           #
#                                                 #
# Eduardo Queiroz Marques-IPAM Canarana-MT/Brazil #
# E-mail: eduardobio2009@gmail.com                #
# 2016 September                                   #
###################################################

#About this Script

#Packages
library(MODIS)
library(raster)
library(rgdal)
library(maptools)
##Load datas (Spera croplands and Araguaia Valley limit)
#Araguaia Valley limit
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Limite Araguaia/")
#arag=readOGR(".","Arag_valley_limit_wgs")
arag<-read.polyfile("Arag_valley_limit_wgs")

##Load EVI Datas
setwd("/mnt/data/dados_publicos/Documents/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI//")

#Make a stack of all EVI datas
xevi=list.files("/mnt/data/dados_publicos/Documents/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI//",pattern="mosaic");xevi
evi = raster::stack(xevi)

##Clip EVI with Araguaia limit
for(i in 1:dim(evi)[3]){
  cat(names(evi[[i]]),format(Sys.time(), "%X"),"\n")
  arag_evi=mask(crop(evi[[i]], arag), arag)
  setwd("/home/eduardo/evi_araguaia")
  name=paste("arag_EVI",substr(names(evi[[i]]),7,20),".tif",sep="")
  writeRaster(arag_evi,name,overwrite=TRUE)
}

#Make a stack of all EVI/Araguaia datas
setwd("/home/eduardo/evi_araguaia")
xevi2=list.files("/home/eduardo/evi_araguaia",pattern=".tif");xevi2
evi2 = raster::stack(xevi2)

##Change value of EVI
evi3<-evi2*0.0001;plot(evi3[[1]])


# tabela files
df=data.frame(file=xevi2,data=as.Date(substr(xevi2,10,20),"%Y.%m.%d"))
head(df)

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

#reclassifying spera to get just soybean, corn and cotton cover types
tab<-c(0,1,2,4,5,6,11,13,99,NA,1,2,NA,3,4,NA,NA,NA);tab<-matrix(tab,9,2);tab
soycorn_sp<-reclassify(spera,tab)

##Clip croplands(spera) with Araguaia limit
arag_croplands <- crop(soycorn_sp, extent(arag))
arag_croplands <- mask(arag_croplands, arag)

##Selection EVI for agricultural year
#soja=list()
##
#files1=subset(df,data>as.Date("2000-08-01")&data<as.Date("2001-08-01"))

#setwd("/home/eduardo/evi_araguaia")

#aa=stack(as.character(files1$file))
#aa2=resample(aa,arag_croplands[[1]])



##MultiStack for Spera years
#Change the arag_crolands extend
arag_cropland2=resample(arag_croplands, evi3[[1]], method='ngb')


#Stacks for agri years
evi.2001<-evi3[[13:35]];evi.spera2001=stack(arag_cropland2[[1]],evi.2001);names(evi.spera2001)
evi.2002<-evi3[[36:58]];evi.spera2002=stack(arag_cropland2[[2]],evi.2002);names(evi.spera2002)
evi.2003<-evi3[[59:81]];evi.spera2003=stack(arag_cropland2[[3]],evi.2003);names(evi.spera2003)
evi.2004<-evi3[[82:104]];evi.spera2004=stack(arag_cropland2[[4]],evi.2004);names(evi.spera2004)
evi.2005<-evi3[[105:127]];evi.spera2005=stack(arag_cropland2[[5]],evi.2005);names(evi.spera2005)
evi.2006<-evi3[[128:150]];evi.spera2006=stack(arag_cropland2[[6]],evi.2006);names(evi.spera2006)
evi.2007<-evi3[[151:173]];evi.spera2007=stack(arag_cropland2[[7]],evi.2007);names(evi.spera2007)
evi.2008<-evi3[[174:196]];evi.spera2008=stack(arag_cropland2[[8]],evi.2008);names(evi.spera2008)
evi.2009<-evi3[[197:219]];evi.spera2009=stack(arag_cropland2[[9]],evi.2009);names(evi.spera2009)
evi.2010<-evi3[[220:242]];evi.spera2010=stack(arag_cropland2[[10]],evi.2010);names(evi.spera2010)
evi.2011<-evi3[[243:265]];evi.spera2011=stack(arag_cropland2[[11]],evi.2011);names(evi.spera2011)
evi.2012<-evi3[[266:288]];evi.spera2012=stack(arag_cropland2[[12]],evi.2012);names(evi.spera2012)
evi.2013<-evi3[[289:311]];evi.spera2013=stack(arag_cropland2[[13]],evi.2013);names(evi.spera2013)
evi.2015<-evi3[[335:344]];evi.spera2015=stack(arag_cropland2[[14]],evi.2015);names(evi.spera2015)

#substituindo NAs por zero
evi.spera2001[is.na(evi.spera2001)] <- 0
#substituir outras classes de uso, diferentes de 1 (soja), por zero
#n?o deu certo
#soy2001=stackApply(evi.spera2001,indices=c(2:24),fun = function(x) ifelse(x[1] <= 1, x, 0))
evi.spera2001[[1]]

##Extract classes
#Soybean
extract(evi3,getValues(arag_cropland2==1),layer=2,method='simple')
#extract(evi.2001,getValues(arag_cropland2==1),layer=2,method='simple')


fun <- function(x) { x[x[[1]]!=1] <- 0; return(x) }
evi.soy.2001 <- calc(evi.spera2001, fun)
evi.soy.2001 = evi.soy.2001[[-1]]
plot(evi.soy.2001)
evi.soy.2002 <- calc(evi.spera2002, fun);evi.soy.2002 = evi.soy.2002[[-1]]
plot(evi.soy.2002)
evi.soy.2003 <- calc(evi.spera2003, fun);evi.soy.2003 = evi.soy.2003[[-1]]
plot(evi.soy.2003)
evi.soy.2004 <- calc(evi.spera2004, fun);evi.soy.2004 = evi.soy.2004[[-1]]
plot(evi.soy.2004)