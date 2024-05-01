##Load datas (Spera croplands and Araguaia Valley limit)
#Araguaia Valley limit
setwd("/mnt/data/dados_publicos/Documents/data_geo/shapes/Limite Araguaia/")
arag=readOGR(".","Arag_valley_limit_wgs")

#Spera Limit full
setwd("~/Documents/public-ipam/data_geo/shapes/Limite da Spera/")
limit_sp=readOGR(".", "limit_spera_latlong")

##Load EVI Datas
setwd("/mnt/data/dados_publicos/Documents/MODIS_local/MATOPIBA/MATOPIBA - EVI_NDVI//")

#Make a stack of all EVI datas
xevi=list.files(,pattern="mosaic");xevi
evi = raster::stack(xevi)

###Clip EVI wich Spera limit
#spera_evi=crop(evi, limit_sp)
#spera_evi <- mask(spera_evi, limit_sp)

##Clip EVI wich Araguaia limit
for(i in 1:dim(evi)[3]){
cat(names(evi[[i]]),format(Sys.time(), "%X"),"\n")
  arag_evi=mask(crop(evi[[i]], arag), arag)
 # arag_evi <- mask(arag_evi, arag)
setwd("/home/eduardo/evi_araguaia")
name=paste("arag_EVI",substr(names(evi[[i]]),7,20),".tif",sep="")
writeRaster(arag_evi,name,overwrite=TRUE)
}

#Spera croplands data
setwd("/mnt/data/dados_publicos/Documents/data_geo/Base_raster/Matopiba_Latlong/Spera_latlong/")
dir()

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

#Change the extend
sp15b=resample(sp15,sp1, method='ngb')

#Make a stack of all spera datas
spera = raster::stack(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp15b)

#reclassifying spera to get just soybean, corn and cotton cover types
tab<-c(0,1,2,4,5,6,11,13,99,0,1,1,0,1,1,0,0,0);tab<-matrix(tab,9,2);tab
soycorn_sp<-reclassify(spera,tab)

#Change resolution
#Maybe do a aggregate before
aggregate()
soycorn_sp2 <- resample(soycorn_sp, arag_evi, method = "ngb")

##Clip croplands(spera) with Araguaia limit
arag_croplands=crop(soycorn_sp, extent(arag))
arag_croplands <- mask(arag_croplands, arag)
