#Extrair dados do Hyperion para toda a area
#Eduardo Q Marques /04/09/2019/

#OBS: Same years are cutted control. So I change control area to other place that the forest is preserved

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#Temporal scale ====================================================================================
h04 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2004", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h05 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2005", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h06 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2006", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h08 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2008", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h10 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2010", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h11 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h12 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2012", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h13 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2013", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C_Hyperion")
area1 = spTransform(area1, crs(h04))

#Extract pixels values ===================================================================
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
colnames(ari1) = c("ari", "parcela")
ari1 = ari1 %>% 
  mutate(data = "2004")
ari1_md = ari1 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[1]], area1[1,]); b3yr <- raster::extract(h05[[1]], area1[2,]); b1yr <- raster::extract(h05[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari2 = as.data.frame(rbind(a, b, c))
ari2 <- ari2[,c(1,3)]
colnames(ari2) = c("ari", "parcela")
ari2 = ari2 %>% 
  mutate(data = "2005")
ari2_md = ari2 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[1]], area1[1,]); b3yr <- raster::extract(h06[[1]], area1[2,]); b1yr <- raster::extract(h06[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari3 = as.data.frame(rbind(a, b, c))
ari3 <- ari3[,c(1,3)]
colnames(ari3) = c("ari", "parcela")
ari3 = ari3 %>% 
  mutate(data = "2006")
ari3_md = ari3 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[1]], area1[1,]); b3yr <- raster::extract(h08[[1]], area1[2,]); b1yr <- raster::extract(h08[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari4 = as.data.frame(rbind(a, b, c))
ari4 <- ari4[,c(1,3)]
colnames(ari4) = c("ari", "parcela")
ari4 = ari4 %>% 
  mutate(data = "2008")
ari4_md = ari4 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[1]], area1[1,]); b3yr <- raster::extract(h10[[1]], area1[2,]); b1yr <- raster::extract(h10[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari5 = as.data.frame(rbind(a, b, c))
ari5 <- ari5[,c(1,3)]
colnames(ari5) = c("ari", "parcela")
ari5 = ari5 %>% 
  mutate(data = "2010")
ari5_md = ari5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[1]], area1[1,]); b3yr <- raster::extract(h11[[1]], area1[2,]); b1yr <- raster::extract(h11[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari6 = as.data.frame(rbind(a, b, c))
ari6 <- ari6[,c(1,3)]
colnames(ari6) = c("ari", "parcela")
ari6 = ari6 %>% 
  mutate(data = "2011")
ari6_md = ari6 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[1]], area1[1,]); b3yr <- raster::extract(h12[[1]], area1[2,]); b1yr <- raster::extract(h12[[1]], area1[3,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari7 = as.data.frame(rbind(a, b, c))
ari7 <- ari7[,c(1,3)]
colnames(ari7) = c("ari", "parcela")
ari7 = ari7 %>% 
  mutate(data = "2012")
ari7_md = ari7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2012")


ari = as.data.frame(rbind(ari1, ari2, ari3, ari4, ari5, ari6, ari7))
ari_md = as.data.frame(rbind(ari1_md, ari2_md, ari3_md, ari4_md, ari5_md, ari6_md, ari7_md))



#Boxplot
ggplot(ari, aes(data,ari, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="ARI")+
  theme_minimal()

#Medians
ggplot(ari_md, aes(data,ari, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="ARI")+
  theme_minimal()

