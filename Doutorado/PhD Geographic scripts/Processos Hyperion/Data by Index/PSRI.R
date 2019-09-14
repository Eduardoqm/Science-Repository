#Extrair dados do Hyperion para toda a area por indice
#Eduardo Q Marques /13/09/2019/

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
crt <- raster::extract(h04[[11]], area1[3,]); b3yr <- raster::extract(h04[[11]], area1[1,]); b1yr <- raster::extract(h04[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri1 = as.data.frame(rbind(a, b, c))
psri1 <- psri1[,c(1,3)]
colnames(psri1) = c("psri", "parcela")
psri1 = psri1 %>% 
  mutate(data = "2004")
psri1_md = psri1 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[11]], area1[3,]); b3yr <- raster::extract(h05[[11]], area1[1,]); b1yr <- raster::extract(h05[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri2 = as.data.frame(rbind(a, b, c))
psri2 <- psri2[,c(1,3)]
colnames(psri2) = c("psri", "parcela")
psri2 = psri2 %>% 
  mutate(data = "2005")
psri2_md = psri2 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[11]], area1[3,]); b3yr <- raster::extract(h06[[11]], area1[1,]); b1yr <- raster::extract(h06[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri3 = as.data.frame(rbind(a, b, c))
psri3 <- psri3[,c(1,3)]
colnames(psri3) = c("psri", "parcela")
psri3 = psri3 %>% 
  mutate(data = "2006")
psri3_md = psri3 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[11]], area1[3,]); b3yr <- raster::extract(h08[[11]], area1[1,]); b1yr <- raster::extract(h08[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri4 = as.data.frame(rbind(a, b, c))
psri4 <- psri4[,c(1,3)]
colnames(psri4) = c("psri", "parcela")
psri4 = psri4 %>% 
  mutate(data = "2008")
psri4_md = psri4 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[11]], area1[3,]); b3yr <- raster::extract(h10[[11]], area1[1,]); b1yr <- raster::extract(h10[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri5 = as.data.frame(rbind(a, b, c))
psri5 <- psri5[,c(1,3)]
colnames(psri5) = c("psri", "parcela")
psri5 = psri5 %>% 
  mutate(data = "2010")
psri5_md = psri5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[11]], area1[3,]); b3yr <- raster::extract(h11[[11]], area1[1,]); b1yr <- raster::extract(h11[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri6 = as.data.frame(rbind(a, b, c))
psri6 <- psri6[,c(1,3)]
colnames(psri6) = c("psri", "parcela")
psri6 = psri6 %>% 
  mutate(data = "2011")
psri6_md = psri6 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[11]], area1[3,]); b3yr <- raster::extract(h12[[11]], area1[1,]); b1yr <- raster::extract(h12[[11]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri7 = as.data.frame(rbind(a, b, c))
psri7 <- psri7[,c(1,3)]
colnames(psri7) = c("psri", "parcela")
psri7 = psri7 %>% 
  mutate(data = "2012")
psri7_md = psri7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2012")


psri = as.data.frame(rbind(psri1, psri2, psri3, psri4, psri5, psri6, psri7))
psri_md = as.data.frame(rbind(psri1_md, psri2_md, psri3_md, psri4_md, psri5_md, psri6_md, psri7_md))



#Boxplot
ggplot(psri, aes(data,psri, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="PSRI")+
  theme_minimal()

#Medians
ggplot(psri_md, aes(data,psri, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="PSRI")+
  theme_minimal()

diff = as.data.frame(cbind(psri1_md, psri2_md, psri3_md, psri4_md, psri5_md, psri6_md, psri7_md))
diff = diff[,c(1,2,5,8,11,14,17,20)]
colnames(diff) = c("parcela","2004","2005","2006","2008","2010","2011","2012")
diff <- as.data.frame(t(diff))
colnames(diff) = c("b1yr","b3yr","controle")
diff <- diff[-c(1), ]

diff$b3yr = as.numeric(as.character(diff$b3yr))
diff$b1yr = as.numeric(as.character(diff$b1yr))
diff$controle = as.numeric(as.character(diff$controle))

diff$b3yr = ((diff$b3yr - diff$controle)/diff$controle)*100
diff$b1yr = ((diff$b1yr - diff$controle)/diff$controle)*100
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no PSRI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()