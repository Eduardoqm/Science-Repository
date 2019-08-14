##################################
#Extrair dados LANDSAT por banda #
#Por: Eduardo Q Marques          #
#08/07/2019                      #
##################################

library(raster)
library(rasterVis)
library(rgdal)
library(reshape2)
library(ggplot2)

#Landsat - 5 ==============================================================================================
#Carregar bandas Landsat
#Listar as bandas (menos a banda 6)
list1 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B1.tif$", full.names=TRUE,recursive=TRUE)
list2 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B2.tif$", full.names=TRUE,recursive=TRUE)
list3 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B3.tif$", full.names=TRUE,recursive=TRUE)
list4 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B4.tif$", full.names=TRUE,recursive=TRUE)
list5 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B5.tif$", full.names=TRUE,recursive=TRUE)
list7 <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Landsat/Landsat5", pattern = "B7.tif$", full.names=TRUE,recursive=TRUE)

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
banda1$data = substr(banda1$data, 2, 9)

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
banda2$data = substr(banda2$data, 2, 9)

#Banda 2 ==================================================================================================
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
banda3$data = substr(banda3$data, 2, 9)

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
banda4$data = substr(banda4$data, 2, 9)

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
banda5$data = substr(banda5$data, 2, 9)

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
banda7$data = substr(banda7$data, 2, 9)

#Graficos =================================================================================================

gg <- melt(banda1, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_point()+ 
  stat_smooth()+
  labs(title="BANDA 1", fill= "Plot",x="Data",y="Reflectância")






ggplot(data=banda1) +
  geom_point(aes(y = control, b3yr, b1yr, x=data))+
  #geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 1",x="Data",y="Reflectância")

ggplot(banda2, aes(y = control, x=data)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 2",x="Data",y="Reflectância")

ggplot(banda3, aes(y = control, x=data)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 3",x="Data",y="Reflectância")

ggplot(banda4, aes(y = control, x=data)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 4",x="Data",y="Reflectância")

ggplot(banda5, aes(y = control, x=data)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 5",x="Data",y="Reflectância")

ggplot(banda7, aes(y = control, x=data)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.5, colour='red')+
  stat_smooth(method="auto")+
  labs(title="BANDA 7",x="Data",y="Reflectância")
