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
crt <- raster::extract(h04[[7]], area1[3,]); b3yr <- raster::extract(h04[[7]], area1[1,]); b1yr <- raster::extract(h04[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi1 = as.data.frame(rbind(a, b, c))
ndvi1 <- ndvi1[,c(1,3)]
colnames(ndvi1) = c("ndvi", "parcela")
ndvi1 = ndvi1 %>% 
  mutate(data = "2004")
ndvi1_md = ndvi1 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[7]], area1[3,]); b3yr <- raster::extract(h05[[7]], area1[1,]); b1yr <- raster::extract(h05[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi2 = as.data.frame(rbind(a, b, c))
ndvi2 <- ndvi2[,c(1,3)]
colnames(ndvi2) = c("ndvi", "parcela")
ndvi2 = ndvi2 %>% 
  mutate(data = "2005")
ndvi2_md = ndvi2 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[7]], area1[3,]); b3yr <- raster::extract(h06[[7]], area1[1,]); b1yr <- raster::extract(h06[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi3 = as.data.frame(rbind(a, b, c))
ndvi3 <- ndvi3[,c(1,3)]
colnames(ndvi3) = c("ndvi", "parcela")
ndvi3 = ndvi3 %>% 
  mutate(data = "2006")
ndvi3_md = ndvi3 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[7]], area1[3,]); b3yr <- raster::extract(h08[[7]], area1[1,]); b1yr <- raster::extract(h08[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi4 = as.data.frame(rbind(a, b, c))
ndvi4 <- ndvi4[,c(1,3)]
colnames(ndvi4) = c("ndvi", "parcela")
ndvi4 = ndvi4 %>% 
  mutate(data = "2008")
ndvi4_md = ndvi4 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[7]], area1[3,]); b3yr <- raster::extract(h10[[7]], area1[1,]); b1yr <- raster::extract(h10[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi5 = as.data.frame(rbind(a, b, c))
ndvi5 <- ndvi5[,c(1,3)]
colnames(ndvi5) = c("ndvi", "parcela")
ndvi5 = ndvi5 %>% 
  mutate(data = "2010")
ndvi5_md = ndvi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[7]], area1[3,]); b3yr <- raster::extract(h11[[7]], area1[1,]); b1yr <- raster::extract(h11[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi6 = as.data.frame(rbind(a, b, c))
ndvi6 <- ndvi6[,c(1,3)]
colnames(ndvi6) = c("ndvi", "parcela")
ndvi6 = ndvi6 %>% 
  mutate(data = "2011")
ndvi6_md = ndvi6 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[7]], area1[3,]); b3yr <- raster::extract(h12[[7]], area1[1,]); b1yr <- raster::extract(h12[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi7 = as.data.frame(rbind(a, b, c))
ndvi7 <- ndvi7[,c(1,3)]
colnames(ndvi7) = c("ndvi", "parcela")
ndvi7 = ndvi7 %>% 
  mutate(data = "2012")
ndvi7_md = ndvi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2012")


ndvi = as.data.frame(rbind(ndvi1, ndvi2, ndvi3, ndvi4, ndvi5, ndvi6, ndvi7))
ndvi_md = as.data.frame(rbind(ndvi1_md, ndvi2_md, ndvi3_md, ndvi4_md, ndvi5_md, ndvi6_md, ndvi7_md))



#Boxplot
ggplot(ndvi, aes(data,ndvi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  theme_minimal()

#Medians
ggplot(ndvi_md, aes(data,ndvi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  theme_minimal()

diff = as.data.frame(cbind(ndvi1_md, ndvi2_md, ndvi3_md, ndvi4_md, ndvi5_md, ndvi6_md, ndvi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no NDVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()