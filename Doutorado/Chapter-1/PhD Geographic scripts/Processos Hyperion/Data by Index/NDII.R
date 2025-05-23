#Extrair dados do Hyperion para toda a area por indice
#Eduardo Q Marques /14/09/2019/

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
crt <- raster::extract(h04[[6]], area1[3,]); b3yr <- raster::extract(h04[[6]], area1[1,]); b1yr <- raster::extract(h04[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii1 = as.data.frame(rbind(a, b, c))
ndii1 <- ndii1[,c(1,3)]
colnames(ndii1) = c("ndii", "parcela")
ndii1 = ndii1 %>% 
  mutate(data = "2004")
ndii1_md = ndii1 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[6]], area1[3,]); b3yr <- raster::extract(h05[[6]], area1[1,]); b1yr <- raster::extract(h05[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii2 = as.data.frame(rbind(a, b, c))
ndii2 <- ndii2[,c(1,3)]
colnames(ndii2) = c("ndii", "parcela")
ndii2 = ndii2 %>% 
  mutate(data = "2005")
ndii2_md = ndii2 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[6]], area1[3,]); b3yr <- raster::extract(h06[[6]], area1[1,]); b1yr <- raster::extract(h06[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii3 = as.data.frame(rbind(a, b, c))
ndii3 <- ndii3[,c(1,3)]
colnames(ndii3) = c("ndii", "parcela")
ndii3 = ndii3 %>% 
  mutate(data = "2006")
ndii3_md = ndii3 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[6]], area1[3,]); b3yr <- raster::extract(h08[[6]], area1[1,]); b1yr <- raster::extract(h08[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii4 = as.data.frame(rbind(a, b, c))
ndii4 <- ndii4[,c(1,3)]
colnames(ndii4) = c("ndii", "parcela")
ndii4 = ndii4 %>% 
  mutate(data = "2008")
ndii4_md = ndii4 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[6]], area1[3,]); b3yr <- raster::extract(h10[[6]], area1[1,]); b1yr <- raster::extract(h10[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii5 = as.data.frame(rbind(a, b, c))
ndii5 <- ndii5[,c(1,3)]
colnames(ndii5) = c("ndii", "parcela")
ndii5 = ndii5 %>% 
  mutate(data = "2010")
ndii5_md = ndii5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[6]], area1[3,]); b3yr <- raster::extract(h11[[6]], area1[1,]); b1yr <- raster::extract(h11[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii6 = as.data.frame(rbind(a, b, c))
ndii6 <- ndii6[,c(1,3)]
colnames(ndii6) = c("ndii", "parcela")
ndii6 = ndii6 %>% 
  mutate(data = "2011")
ndii6_md = ndii6 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[6]], area1[3,]); b3yr <- raster::extract(h12[[6]], area1[1,]); b1yr <- raster::extract(h12[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii7 = as.data.frame(rbind(a, b, c))
ndii7 <- ndii7[,c(1,3)]
colnames(ndii7) = c("ndii", "parcela")
ndii7 = ndii7 %>% 
  mutate(data = "2012")
ndii7_md = ndii7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2012")


ndii = as.data.frame(rbind(ndii1, ndii2, ndii3, ndii4, ndii5, ndii6, ndii7))
ndii_md = as.data.frame(rbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))



#Boxplot
ggplot(ndii, aes(data,ndii, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NDII")+
  theme_minimal()

#Medians
ggplot(ndii_md, aes(data,ndii, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDII")+
  theme_minimal()

diff = as.data.frame(cbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no NDII)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()