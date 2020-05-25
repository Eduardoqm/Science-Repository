#Calculate index with Hyperion satellite image
#Eduardo Q Marques (eduardobio2009@gmail.com) 21/08/2019

library(raster)
library(rasterVis)

setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Hyperion")
a <- stack("Reflectance_09_Aug_2004")

#a = a/10000 #Normilze to calculate EVIs

levelplot(a[[4]], margin = FALSE, par.settings = RdBuTheme)

for (x in 1:152) {
  print(a[[x]]@data@names)
}




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




levelplot(evi, margin = FALSE, par.settings = RdBuTheme)

#writeRaster(ndvi, filename="ndvi2008.tiff", overwrite=TRUE)
