#Calculate index with Hyperion satellite image
#Eduardo Q Marques (eduardobio2009@gmail.com) 21/08/2019

library(raster)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion/Registro hyperion/Cortadas")
a <- stack("Reg_20_july_2013.tif")

a = a/10000 #Normilze to calculate EVIs


#Vegetation Index - Structure
evi = 2.5*((a[[44]]-a[[24]])/(a[[44]]+6*a[[24]]-7.5*a[[5]]+1))

evi2 = 2.5*((a[[44]]-a[[24]])/(a[[44]]+2.4*a[[24]]+1))

ndvi = (a[[44]]-a[[24]])/(a[[44]]+a[[24]])

vari = (a[[14]]-a[[24]])/(a[[14]]+a[[24]]-a[[7]])

vig = (a[[14]]-a[[24]])/(a[[14]]+a[[24]])

nirv = a[[44]]*ndvi


#Vegetation Index - Biochemistry
ari = (1/a[[14]])-(1/a[[30]])

lwvi2 = (a[[63]]-a[[74]])/(a[[63]]+a[[74]])

msi = a[[100]]/a[[40]]

ndii = (a[[40]]-a[[105]])/(a[[40]]+a[[105]])

ndwi = (a[[43]]-a[[78]])/(a[[43]]+a[[78]])

pssr = a[[38]]/a[[25]]

psri = (a[[26]]-a[[8]])/a[[33]]

sipi = (a[[38]]-a[[5]])/(a[[38]]+a[[26]])

wbi = a[[48]]/a[[51]]


#Vegetation Index - Physiology
pri = (a[[11]]-a[[15]])/(a[[11]]+a[[15]])

rendvi = (a[[33]]-a[[28]])/(a[[33]]+a[[28]])


#Salve index
setwd('C:\\Users\\Eduardo Q Marques\\Documents\\My Jobs\\Doutorado\\Deposito\\Banco de Dados Tanguro\\Tanguro Indices\\Hyperion\\2013')
writeRaster(evi, filename="evi-2013.tiff", overwrite=TRUE)
writeRaster(evi2, filename="evi2-2013.tiff", overwrite=TRUE)
writeRaster(ndvi, filename="ndvi-2013.tiff", overwrite=TRUE)
writeRaster(vari, filename="vari-2013.tiff", overwrite=TRUE)
writeRaster(vig, filename="vig-2013.tiff", overwrite=TRUE)
writeRaster(nirv, filename="nirv-2013.tiff", overwrite=TRUE)
writeRaster(ari, filename="ari-2013.tiff", overwrite=TRUE)
writeRaster(lwvi2, filename="lwvi2-2013.tiff", overwrite=TRUE)
writeRaster(msi, filename="msi-2013.tiff", overwrite=TRUE)
writeRaster(ndii, filename="ndii-2013.tiff", overwrite=TRUE)
writeRaster(ndwi, filename="ndwi-2013.tiff", overwrite=TRUE)
writeRaster(pssr, filename="pssr-2013.tiff", overwrite=TRUE)
writeRaster(psri, filename="psri-2013.tiff", overwrite=TRUE)
writeRaster(sipi, filename="sipi-2013.tiff", overwrite=TRUE)
writeRaster(wbi, filename="wbi-2013.tiff", overwrite=TRUE)
writeRaster(pri, filename="pri-2013.tiff", overwrite=TRUE)
writeRaster(rendvi, filename="rendvi-2013.tiff", overwrite=TRUE)

