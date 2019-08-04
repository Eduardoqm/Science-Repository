#Fazer Stack com diferente extents
#Eduardo Q Marques
#11/07/2019

library(raster)
#Landsat - 5 ==============================================================================================
#Antes de fazermos a lista para o stack, precisamos cortar as bandas para ficaram no mesmo extent
#Abrir cena cortada para referencia
ref <- raster('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Extent_raster/Extent_raster.tif')
e <- extent(ref)
#Criar uma pasta para salvar as bandas recortadas
outpath <- "C:/Users/Eduardo Q Marques/Documents/landsatcrop5/"
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