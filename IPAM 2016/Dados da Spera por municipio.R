## corrige eerro em ler polygon dataframes
# muda acentos para pontos

Sys.setlocale(category = "LC_ALL", locale = "C")

# carregar library
library(ggmap)
library(raster)
library(maptools)
library(spatial.tools)
library(snow)
# Diretorio do shape
setwd("/home/eduardo/Documents/public-ipam/data_geo/shapes/municipios_IBGE/municipios_dissolve")
dir()
# shape dos municipios
mun2001 <-readOGR(".",layer="Mtopiba_2001")
mun2001
#P4S <- CRS("+proj=longlat +datum=WGS84")
#utm <-CRS("+proj=utm +zone=22 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

#tang_plano<-spTransform(tang,utm)

#plot(tang_plano)

# diretorio dos dados da Spera
setwd("D:/IPAM/Dados coletados/Dados Spera/Mato Grosso (Cerrado)")
pasta="D:/IPAM/Dados coletados/Dados Spera/Mato Grosso (Cerrado)/Dados Spera por muncipio"

sp2001=raster("GY2001.tif")

tang2001<-crop(sp2001,tang_plano)
tang2001_mask<-mask(tang2001,tang_plano)

#reclassifying sp2001 to get just soybean cover types
table(values(sp2001_mask)) #conferindo tipos de cobertura presentes no raster
tab<-c(0,1,2,4,5,6,11,13,99,NA,1,NA,NA,5,6,NA,NA,NA);tab<-matrix(tab,9,2);tab
sp2001_soja<-reclassify(tang2001_mask,tab)

#another way to reclass
#ff=function(x){x[x==0]=NA;x[x==2]=NA;x[x==4]=NA;x[x==11]=NA;x[x==13]=NA;x[x==99]=NA;return(x)}
#soja=calc(sp2001,fun=ff)
x11()
plot(sp2001_soja)

start.time <- Sys.time()
beginCluster( detectCores() ) #use all but one core

tang_plano$SoyArea2001 <- extract(sp2001_soja, tang_plano, fun = sum, na.rm = TRUE,cellnumbers=TRUE)

endCluster()

#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

head(mumt@data)

arquivo = foreach(i = 1:100 )%dopar%{
  x.temp = subset(shape, XX == i)
  extract(x.temp, RASTER)
  
}
#########################################
##Quantas c?lulas de soja existem em cada talh?o da Tanguro entre os anos de 2001 a 2013?
#########################################
# diretorio dos dados da Spera
setwd("D:/IPAM/Dados coletados/Dados Spera/Mato Grosso (Cerrado)/only_tif")
#pasta="D:/IPAM/Dados coletados/Dados Spera/Mato Grosso (Cerrado)/only_tif"

x1=list.files(,pattern="*.tif");x1
spera = stack(x1)

tang_sp<-crop(spera,tang_plano)
tang_sp_mask<-mask(tang_sp,tang_plano)

#reclassifying sp2001 to get just soybean cover types
table(values(tang_sp_mask)) #conferindo tipos de cobertura presentes no raster
tab<-c(0,1,2,4,5,6,11,13,99,NA,1,NA,NA,5,6,NA,NA,NA);tab<-matrix(tab,9,2);tab
tang_soja<-reclassify(tang_sp_mask,tab)

#another way to reclass
#ff=function(x){x[x==0]=NA;x[x==2]=NA;x[x==4]=NA;x[x==11]=NA;x[x==13]=NA;x[x==99]=NA;return(x)}
#soja=calc(sp2001,fun=ff)
x11()
plot(tang_soja)

start.time <- Sys.time()
beginCluster( detectCores() ) #use all but one core

tang_plano$SoyArea <- extract(tang_soja, tang_plano, fun = sum, na.rm = TRUE,cellnumbers=TRUE)

endCluster()
barplot(tang_plano@data$SoyArea)
pixels<-data.frame(tang_plano@data$SoyArea)
area<-pixels*53.4
barplot(as.matrix(area))