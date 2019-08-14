##################################
#Extrair dados LANDSAT por banda #
#Por: Eduardo Q Marques          #
#08/07/2019                      #
##################################

library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(dplyr)
library(ggplot2)

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

#Abrir os shapes para amostrar pixels (100 pontos por shape)
crt <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Controle_points")
b1yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B1y_pontos")
b3yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B3y_pontos")

#Extrair valor dos pixels
#Banda 1 ==================================================================================================
crtdf <- extract(b1, crt)
b1yrdf <- extract(b1, b1yr)
b3yrdf <- extract(b1, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda1 <- banda[,c(2, 3, 6, 9)]
banda1$data = substr(banda1$data, 18, 25)

#Banda 2 ==================================================================================================
crtdf <- extract(b2, crt)
b1yrdf <- extract(b2, b1yr)
b3yrdf <- extract(b2, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda2 <- banda[,c(2, 3, 6, 9)]
banda2$data = substr(banda2$data, 18, 25)

#Banda 3 ==================================================================================================
crtdf <- extract(b3, crt)
b1yrdf <- extract(b3, b1yr)
b3yrdf <- extract(b3, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda3 <- banda[,c(2, 3, 6, 9)]
banda3$data = substr(banda3$data, 18, 25)

#Banda 4 ==================================================================================================
crtdf <- extract(b4, crt)
b1yrdf <- extract(b4, b1yr)
b3yrdf <- extract(b4, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda4 <- banda[,c(2, 3, 6, 9)]
banda4$data = substr(banda4$data, 18, 25)

#Banda 5 ==================================================================================================
crtdf <- extract(b5, crt)
b1yrdf <- extract(b5, b1yr)
b3yrdf <- extract(b5, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda5 <- banda[,c(2, 3, 6, 9)]
banda5$data = substr(banda5$data, 18, 25)

#Banda 7 ==================================================================================================
crtdf <- extract(b7, crt)
b1yrdf <- extract(b7, b1yr)
b3yrdf <- extract(b7, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda7 <- banda[,c(2, 3, 6, 9)]
banda7$data = substr(banda7$data, 18, 25)



#Landsat - 8 ==============================================================================================



#Antes de fazermos a lista para o stack, precisamos cortar as bandas para ficaram no mesmo extent
#Aqui o arquivo do extent já está pronto, é só puxar os dados e rodar a funcao

#Criar uma pasta para salvar as bandas recortadas
outpath <- "C:/Users/Eduardo Q Marques/Documents/landsatcrop8/"
dir.create(outpath)

#Criar lista com as bandas
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat8")
files <- list.files(pattern=".tif$") 

#Adicionar o diretorio de saida e adicionar extencao do arquivo
outfiles <- paste0(outpath, files)
extension(outfiles) <- 'tif'

#Funcao de corte
for(i in 1:length(files)) {
  r <-raster(files[i])
  rc <- crop(r, e)
  rc <- writeRaster(rc, outfiles[i])
}

#Carregar bandas Landsat cortadas============================================================================
#Listar as bandas (banda 2 ate a banda 7 são equivalentes as bandas do landsat5)
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

#Extrair valor dos pixels
#Banda 2 ==================================================================================================
crtdf <- extract(b2, crt)
b1yrdf <- extract(b2, b1yr)
b3yrdf <- extract(b2, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda2b <- banda[,c(2, 3, 6, 9)]
banda2b$data = substr(banda2b$data, 18, 25)

#Banda 3 ==================================================================================================
crtdf <- extract(b3, crt)
b1yrdf <- extract(b3, b1yr)
b3yrdf <- extract(b3, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda3b <- banda[,c(2, 3, 6, 9)]
banda3b$data = substr(banda3b$data, 18, 25)

#Banda 4 ==================================================================================================
crtdf <- extract(b4, crt)
b1yrdf <- extract(b4, b1yr)
b3yrdf <- extract(b4, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda4b <- banda[,c(2, 3, 6, 9)]
banda4b$data = substr(banda4b$data, 18, 25)

#Banda 5 ==================================================================================================
crtdf <- extract(b5, crt)
b1yrdf <- extract(b5, b1yr)
b3yrdf <- extract(b5, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda5b <- banda[,c(2, 3, 6, 9)]
banda5b$data = substr(banda5b$data, 18, 25)

#Banda 6 ==================================================================================================
crtdf <- extract(b6, crt)
b1yrdf <- extract(b6, b1yr)
b3yrdf <- extract(b6, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda6b <- banda[,c(2, 3, 6, 9)]
banda6b$data = substr(banda6b$data, 18, 25)

#Banda 7 ==================================================================================================
crtdf <- extract(b7, crt)
b1yrdf <- extract(b7, b1yr)
b3yrdf <- extract(b7, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

banda = as.data.frame(cbind(a, b, c))
banda7b <- banda[,c(2, 3, 6, 9)]
banda7b$data = substr(banda7b$data, 18, 25)


#Uniao das bandas equivalentes do Landsat5 e Landsat8=============================================================
#----------------------------------------------#
#OBS: Unimos as bandas TM e OLI na sequencia:  #
#B1/B2 = Azul                                  #
#B2/B3 = Verde                                 #
#B3/B4 = Vermelho                              #
#B4/B5 = IR Proximo                            #
#B5/B6 = IR Medio                              #
#B7/B7 = IR Medio                              #
#----------------------------------------------#
azul = as.data.frame(rbind(banda1, banda2b))
verde = as.data.frame(rbind(banda2, banda3b))
red = as.data.frame(rbind(banda3, banda4b))
irprox = as.data.frame(rbind(banda4, banda5b))
irmedio = as.data.frame(rbind(banda5, banda6b))
irmedio2 = as.data.frame(rbind(banda7, banda7b))

#Media dos valores das bandas por data
azul_m = azul %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

verde_m = verde %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

red_m = red %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

irprox_m = irprox %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

irmedio_m = irmedio %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

irmedio2_m = irmedio2 %>%
  group_by(data) %>% 
  summarise(controle = mean(controle),
            b3yr = mean(b3yr),
            b1yr = mean(b1yr))

#Graficos =================================================================================================
#Azul
gg <- melt(azul_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (Azul)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))

#Verde
gg <- melt(verde_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (Verde)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))

#Red
gg <- melt(red_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (Vermelho)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))

#Infravermelho proximo
gg <- melt(irprox_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (IR Próximo)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))

#Infravermelho medio
gg <- melt(irmedio_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (IR Médio)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))

#Infravermelho medio
gg <- melt(irmedio2_m, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="BANDA (IR Médio 2)", fill= "Plot",x="Data",y="Reflectância")+
  theme(axis.text.x = element_text(angle = 90))