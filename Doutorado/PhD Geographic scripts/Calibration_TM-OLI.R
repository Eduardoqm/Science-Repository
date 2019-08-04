#Calibration TM and OLI
library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(dplyr)

#Landsat - 5 ==============================================================================================
#Antes de fazermos a lista para o stack, precisamos cortar as bandas para ficaram no mesmo extent
#Abrir cena cortada para referencia
ref <- raster('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Extent_raster/Extent_calib.tif')
e <- extent(ref)
#Criar uma pasta para salvar as bandas recortadas
outpath <- "C:/Users/Eduardo Q Marques/Documents/Calib5/"
dir.create(outpath)

#Criar lista com as bandas
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5")
files <- list.files(pattern=".tif$") 

#Adicionar o diretorio de saida e adicionar extencao do arquivo
outfiles <- paste0(outpath, files)
extension(outfiles) <- 'tif'

#Função de corte
for(i in 1:length(files)) {
  r <-raster(files[i])
  rc <- crop(r, e)
  rc <- writeRaster(rc, outfiles[i])
}

#Carregar bandas Landsat cortadas============================================================================
#Listar as bandas (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib5", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Fazer os stacks das bandas
b1 <- stack(list1)
b2 <- stack(list2)
b3 <- stack(list3)
b4 <- stack(list4)
b5 <- stack(list5)
b7 <- stack(list7)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
po <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Amostra_calib")

#Extrair valor dos pixels
crtdf <- extract(b1, po)
tm1 <- melt(crtdf)
colnames(tm1) = c('id', 'data', 'controle')

crtdf <- extract(b2, po)
tm2 <- melt(crtdf)
colnames(tm2) = c('id', 'data', 'controle')

crtdf <- extract(b3, po)
tm3 <- melt(crtdf)
colnames(tm3) = c('id', 'data', 'controle')

crtdf <- extract(b4, po)
tm4 <- melt(crtdf)
colnames(tm4) = c('id', 'data', 'controle')

crtdf <- extract(b5, po)
tm5 <- melt(crtdf)
colnames(tm5) = c('id', 'data', 'controle')

crtdf <- extract(b7, po)
tm7 <- melt(crtdf)
colnames(tm7) = c('id', 'data', 'controle')



#Landsat - 8 ==============================================================================================
#Antes de fazermos a lista para o stack, precisamos cortar as bandas para ficaram no mesmo extent
#Abrir cena cortada para referencia
ref <- raster('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Extent_raster/Extent_calib.tif')
e <- extent(ref)
#Criar uma pasta para salvar as bandas recortadas
outpath <- "C:/Users/Eduardo Q Marques/Documents/Calib8/"
dir.create(outpath)

#Criar lista com as bandas
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat8")
files <- list.files(pattern=".tif$") 

#Adicionar o diretorio de saida e adicionar extencao do arquivo
outfiles <- paste0(outpath, files)
extension(outfiles) <- 'tif'

#Função de corte
for(i in 1:length(files)) {
  r <-raster(files[i])
  rc <- crop(r, e)
  rc <- writeRaster(rc, outfiles[i])
}

#Carregar bandas Landsat cortadas============================================================================
#Listar as bandas (banda 2 ate a banda 7 são equivalentes as bandas do landsat5)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list6 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band6.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/Calib8", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Fazer os stacks das bandas
b2 <- stack(list2)
b3 <- stack(list3)
b4 <- stack(list4)
b5 <- stack(list5)
b6 <- stack(list6)
b7 <- stack(list7)

#Extrair valor dos pixels
crtdf <- extract(b2, po)
oli2 <- melt(crtdf)
colnames(oli2) = c('id', 'data', 'controle')

crtdf <- extract(b3, po)
oli3 <- melt(crtdf)
colnames(oli3) = c('id', 'data', 'controle')

crtdf <- extract(b4, po)
oli4 <- melt(crtdf)
colnames(oli4) = c('id', 'data', 'controle')

crtdf <- extract(b5, po)
oli5 <- melt(crtdf)
colnames(oli5) = c('id', 'data', 'controle')

crtdf <- extract(b6, po)
oli6 <- melt(crtdf)
colnames(oli6) = c('id', 'data', 'controle')

crtdf <- extract(b7, po)
oli7 <- melt(crtdf)
colnames(oli7) = c('id', 'data', 'controle')

#Getting the calibration value================================================================================
azul = (median(tm1$controle) - median(oli2$controle))
verde = (median(tm2$controle) - median(oli3$controle))
red = (median(tm3$controle) - median(oli4$controle))
irprox = (median(tm4$controle) - median(oli5$controle))
irmedio = (median(tm5$controle) - median(oli6$controle))
irmedio2 = (median(tm7$controle) - median(oli7$controle))

#Doing the calibration
#Carregar bandas Landsat cortadas============================================================================
#Listar as bandas (banda 2 ate a banda 7 são equivalentes as bandas do landsat5)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list6 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band6.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop_oli", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Fazer os stacks das bandas
b2 <- stack(list2)
b3 <- stack(list3)
b4 <- stack(list4)
b5 <- stack(list5)
b6 <- stack(list6)
b7 <- stack(list7)

#Formula
oli2 <- b2 + azul
oli3 <- b3 + verde
oli4 <- b4 + red
oli5 <- b5 + irprox
oli6 <- b6 + irmedio
oli7 <- b7 + irmedio2

#Salve calibrated rasters-----------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/landsatcrop8")

for (i in 1:length(oli2)) {
  writeRaster(oli2[[i]], names(b2[[i]]), format = "GTiff", bylayer = FALSE)
}
for (i in 1:length(oli3)) {
  writeRaster(oli3[[i]], names(b3[[i]]), format = "GTiff", bylayer = FALSE)
}
for (i in 1:length(oli4)) {
  writeRaster(oli4[[i]], names(b4[[i]]), format = "GTiff", bylayer = FALSE)
}
for (i in 1:length(oli5)) {
  writeRaster(oli5[[i]], names(b5[[i]]), format = "GTiff", bylayer = FALSE)
}
for (i in 1:length(oli6)) {
  writeRaster(oli6[[i]], names(b6[[i]]), format = "GTiff", bylayer = FALSE)
}
for (i in 1:length(oli7)) {
  writeRaster(oli7[[i]], names(b7[[i]]), format = "GTiff", bylayer = FALSE)
}







