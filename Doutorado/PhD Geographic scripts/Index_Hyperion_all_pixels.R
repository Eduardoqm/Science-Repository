#Extrair dados do Hyperion para toda a area
#Eduardo Q Marques /04/09/2019/

#OBS: Same years are cutted control. So I change control area to other place that the forest is preserved

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#Temporal scale====================================================================================
h04 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2004", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h05 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2005", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h06 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2006", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h08 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2008", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h10 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2010", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h11 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h12 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2012", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h13 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2013", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Abrir os shapes para amostrar pixels (100 pontos por shape)
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C_Hyperion")
area1 = spTransform(area1, crs(h04))

#Extrair valor dos pixels
#ARI
crt <- raster::extract(h04[[1]], area1[1,]); b3yr <- raster::extract(h04[[1]], area1[2,]); b1yr <- raster::extract(h04[[1]], area1[3,])


a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")


ari1 = as.data.frame(rbind(a, b, c))
ari1 <- ari1[,c(1,3)]
colnames(ari1) = c("ari_2004", "parcela")





  
crt <- raster::extract(h05[[1]], area1[1,]); b3yr <- raster::extract(h05[[1]], area1[2,]); b1yr <- raster::extract(h05[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
ari2 = as.data.frame(cbind(a, b, c))
  
ari = as.data.frame(rbind(ari1, ari2))





a <- melt(crt)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yr)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yr)
colnames(b) = c('id', 'data', 'b3yr')

df = as.data.frame(cbind(a, b, c))
df <- df[,c(2, 3, 7, 11)]

df_md <- df

df_md = df_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

#Plots
gg <- melt(df_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
