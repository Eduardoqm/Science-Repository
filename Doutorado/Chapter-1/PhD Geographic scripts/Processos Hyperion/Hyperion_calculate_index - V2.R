#Calculate index with Hyperion satellite image - V2

#Eduardo Q Marques  14/07/2021

library(raster)

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/EO-1/Cortadas")
dir()

list1 = list.files()

for (j in 1:7) {
  setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/EO-1/Cortadas")
  a <- stack(list1[j])
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
  
  ndii = (a[[44]]-a[[105]])/(a[[44]]+a[[105]]) #In version-1 I used band 40, but here i am trying 44
  
  ndwi = (a[[43]]-a[[78]])/(a[[43]]+a[[78]])
  
  pssr = a[[38]]/a[[25]]
  
  psri = (a[[26]]-a[[8]])/a[[33]]
  
  sipi = (a[[38]]-a[[5]])/(a[[38]]+a[[26]])
  
  wbi = a[[48]]/a[[51]]
  
  
  #Vegetation Index - Physiology
  pri = (a[[11]]-a[[15]])/(a[[11]]+a[[15]])
  
  rendvi = (a[[33]]-a[[28]])/(a[[33]]+a[[28]])
  
  #Vegetation Index - Fire
  nbr = (a[[105]]-a[[138]])/(a[[105]]+a[[138]])
  nbr2 = (a[[40]]-a[[138]])/(a[[40]]+a[[138]])
  
  
  #Salve index
  setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Hyperion')
  
  name1 = names(a[[1]])
  name1 = substr(name1, 12, 19)
  
  writeRaster(evi, filename=paste("evi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(evi2, filename=paste("evi2", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(ndvi, filename=paste("ndvi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(vari, filename=paste("vari", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(vig, filename=paste("vig", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(nirv, filename=paste("nirv", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(ari, filename=paste("ari", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(lwvi2, filename=paste("lwvi2", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(msi, filename=paste("msi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(ndii, filename=paste("ndii", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(ndwi, filename=paste("ndwi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(pssr, filename=paste("pssr", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(psri, filename=paste("psri", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(sipi, filename=paste("sipi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(wbi, filename=paste("wbi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(pri, filename=paste("pri", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(rendvi, filename=paste("rendvi", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(nbr, filename=paste("nbr", name1, sep = "-", collapse = NULL), format = "GTiff")
  writeRaster(nbr2, filename=paste("nbr2", name1, sep = "-", collapse = NULL), format = "GTiff")
}





