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
crt <- raster::extract(h04[[9]], area1[3,]); b3yr <- raster::extract(h04[[9]], area1[1,]); b1yr <- raster::extract(h04[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv1 = as.data.frame(rbind(a, b, c))
nirv1 <- nirv1[,c(1,3)]
colnames(nirv1) = c("nirv", "parcela")
nirv1 = nirv1 %>% 
  mutate(data = "2004")
nirv1_md = nirv1 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[9]], area1[3,]); b3yr <- raster::extract(h05[[9]], area1[1,]); b1yr <- raster::extract(h05[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv2 = as.data.frame(rbind(a, b, c))
nirv2 <- nirv2[,c(1,3)]
colnames(nirv2) = c("nirv", "parcela")
nirv2 = nirv2 %>% 
  mutate(data = "2005")
nirv2_md = nirv2 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[9]], area1[3,]); b3yr <- raster::extract(h06[[9]], area1[1,]); b1yr <- raster::extract(h06[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv3 = as.data.frame(rbind(a, b, c))
nirv3 <- nirv3[,c(1,3)]
colnames(nirv3) = c("nirv", "parcela")
nirv3 = nirv3 %>% 
  mutate(data = "2006")
nirv3_md = nirv3 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[9]], area1[3,]); b3yr <- raster::extract(h08[[9]], area1[1,]); b1yr <- raster::extract(h08[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv4 = as.data.frame(rbind(a, b, c))
nirv4 <- nirv4[,c(1,3)]
colnames(nirv4) = c("nirv", "parcela")
nirv4 = nirv4 %>% 
  mutate(data = "2008")
nirv4_md = nirv4 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[9]], area1[3,]); b3yr <- raster::extract(h10[[9]], area1[1,]); b1yr <- raster::extract(h10[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv5 = as.data.frame(rbind(a, b, c))
nirv5 <- nirv5[,c(1,3)]
colnames(nirv5) = c("nirv", "parcela")
nirv5 = nirv5 %>% 
  mutate(data = "2010")
nirv5_md = nirv5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[9]], area1[3,]); b3yr <- raster::extract(h11[[9]], area1[1,]); b1yr <- raster::extract(h11[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv6 = as.data.frame(rbind(a, b, c))
nirv6 <- nirv6[,c(1,3)]
colnames(nirv6) = c("nirv", "parcela")
nirv6 = nirv6 %>% 
  mutate(data = "2011")
nirv6_md = nirv6 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[9]], area1[3,]); b3yr <- raster::extract(h12[[9]], area1[1,]); b1yr <- raster::extract(h12[[9]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv7 = as.data.frame(rbind(a, b, c))
nirv7 <- nirv7[,c(1,3)]
colnames(nirv7) = c("nirv", "parcela")
nirv7 = nirv7 %>% 
  mutate(data = "2012")
nirv7_md = nirv7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2012")


nirv = as.data.frame(rbind(nirv1, nirv2, nirv3, nirv4, nirv5, nirv6, nirv7))
nirv_md = as.data.frame(rbind(nirv1_md, nirv2_md, nirv3_md, nirv4_md, nirv5_md, nirv6_md, nirv7_md))



#Boxplot
ggplot(nirv, aes(data,nirv, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="NIRV")+
  theme_minimal()

#Medians
ggplot(nirv_md, aes(data,nirv, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NIRV")+
  theme_minimal()

diff = as.data.frame(cbind(nirv1_md, nirv2_md, nirv3_md, nirv4_md, nirv5_md, nirv6_md, nirv7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no NIRV)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()