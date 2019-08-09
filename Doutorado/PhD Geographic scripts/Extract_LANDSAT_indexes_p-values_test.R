##################################
#Extrair dados LANDSAT por banda #
#para gerar planilha de teste de #
#p-values                        #
#                                #
#Por: Eduardo Q Marques          #
#07/08/2019                      #
##################################

library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(dplyr)
library(ggplot2)

#NDVI ===========================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

ndvi <- stack(list)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
crt <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Controle_points")
b1yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B1y_pontos")
b3yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B3y_pontos")

#Extrair valor dos pixels
crtdf <- extract(ndvi, crt)
b1yrdf <- extract(ndvi, b1yr)
b3yrdf <- extract(ndvi, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

ndvi = as.data.frame(cbind(a, b, c))
ndvi <- ndvi[,c(2, 3, 6, 9)]
ndvi$data = substr(ndvi$data, 18, 25)


#EVI ========================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

evi2 <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(evi2, crt)
b1yrdf <- extract(evi2, b1yr)
b3yrdf <- extract(evi2, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

evi2 = as.data.frame(cbind(a, b, c))
evi2 <- evi2[,c(2, 3, 6, 9)]
evi2$data = substr(evi2$data, 18, 25)

#Juntar indices
ndvi = melt(ndvi)
colnames(ndvi) = c('ano', 'plot', 'ndvi')
evi2 = melt(evi2)
colnames(evi2) = c('ano', 'plot', 'evi')

index = as.data.frame(cbind(ndvi, evi2))
index = index[,c(1,2,3,6)]

#Salve the master data frame
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/INPE (Temporada 2019)/Graficos landsat/Acuracia do Landsat")
write.table(index,"Master_Landsat_index.csv", sep=",")













