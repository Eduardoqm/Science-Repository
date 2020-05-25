#Calibration TM and OLI
library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(dplyr)

#Landsat - 5 ==================================================================================
#Listar as bandas (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop5", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Fazer os stacks das bandas
b1 <- stack(list1)
b2 <- stack(list2)
b3 <- stack(list3)
b4 <- stack(list4)
b5 <- stack(list5)
b7 <- stack(list7)

#Poligono usado para calibracao
#calib_ext = drawExtent()
#dput(calib_ext)
calib_ext = new("Extent", xmin = 348032.99424017, xmax = 349945.946574669, 
                ymin = -1448651.82005346, ymax = -1447150.39606642)

b1 <- crop(b1, calib_ext)
b2 <- crop(b2, calib_ext)
b3 <- crop(b3, calib_ext)
b4 <- crop(b4, calib_ext)
b5 <- crop(b5, calib_ext)
b7 <- crop(b7, calib_ext)

tm_b1 <- median(b1[])
tm_b2 <- median(b2[])
tm_b3 <- median(b3[])
tm_b4 <- median(b4[])
tm_b5 <- median(b5[])
tm_b7 <- median(b7[])



#Landsat - 8 ============================================================================
#Listar as bandas (banda 2 ate a banda 7 sÃ£o equivalentes as bandas do landsat5)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band5.tif$", full.names=TRUE,recursive=TRUE)
list6 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band6.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/landsatcrop8", pattern = "band7.tif$", full.names=TRUE,recursive=TRUE)

#Fazer os stacks das bandas
b2 <- stack(list2)
b3 <- stack(list3)
b4 <- stack(list4)
b5 <- stack(list5)
b6 <- stack(list6)
b7 <- stack(list7)


b2 <- crop(b2, calib_ext)
b3 <- crop(b3, calib_ext)
b4 <- crop(b4, calib_ext)
b5 <- crop(b5, calib_ext)
b6 <- crop(b6, calib_ext)
b7 <- crop(b7, calib_ext)


oli_b2 <- median(b2[])
oli_b3 <- median(b3[])
oli_b4 <- median(b4[])
oli_b5 <- median(b5[])
oli_b6 <- median(b6[])
oli_b7 <- median(b7[])

#Getting the calibration value======================================================
azul = tm_b1 - oli_b2
verde = tm_b2 - oli_b3
red = tm_b3 - oli_b4
irprox = tm_b4 - oli_b5
irmedio = tm_b5 - oli_b6
irmedio2 = tm_b7 - oli_b7

#Doing the calibration
#Fazer os stacks das bandas OLI
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
setwd("C:/Users/Eduardo Q Marques/Documents/landsatcrop8b")

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

#FunÃ§Ã£o de corte
for(i in 1:length(files)) {
  r <-raster(files[i])
  rc <- crop(r, e)
  rc <- writeRaster(rc, outfiles[i])
}

#Carregar bandas Landsat cortadas============================================================================
#Listar as bandas (banda 2 ate a banda 7 sÃ£o equivalentes as bandas do landsat5)
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
#Listar as bandas (banda 2 ate a banda 7 sÃ£o equivalentes as bandas do landsat5)
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







