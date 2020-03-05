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
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Borda_nucleo_hyperion")
area1 = spTransform(area1, crs(h04))

#Extract pixels values ===================================================================
crt <- raster::extract(h04[[5]], area1[3,]); b3yr <- raster::extract(h04[[5]], area1[1,]); b1yr <- raster::extract(h04[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi1 = as.data.frame(rbind(a, b, c))
msi1 <- msi1[,c(1,3)]
colnames(msi1) = c("msi", "parcela")
msi1 = msi1 %>% 
  mutate(data = "2004")
msi1_md = msi1 %>%
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[5]], area1[3,]); b3yr <- raster::extract(h05[[5]], area1[1,]); b1yr <- raster::extract(h05[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi2 = as.data.frame(rbind(a, b, c))
msi2 <- msi2[,c(1,3)]
colnames(msi2) = c("msi", "parcela")
msi2 = msi2 %>% 
  mutate(data = "2005")
msi2_md = msi2 %>%
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[5]], area1[3,]); b3yr <- raster::extract(h06[[5]], area1[1,]); b1yr <- raster::extract(h06[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi3 = as.data.frame(rbind(a, b, c))
msi3 <- msi3[,c(1,3)]
colnames(msi3) = c("msi", "parcela")
msi3 = msi3 %>% 
  mutate(data = "2006")
msi3_md = msi3 %>%
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[5]], area1[3,]); b3yr <- raster::extract(h08[[5]], area1[1,]); b1yr <- raster::extract(h08[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi4 = as.data.frame(rbind(a, b, c))
msi4 <- msi4[,c(1,3)]
colnames(msi4) = c("msi", "parcela")
msi4 = msi4 %>% 
  mutate(data = "2008")
msi4_md = msi4 %>%
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[5]], area1[3,]); b3yr <- raster::extract(h10[[5]], area1[1,]); b1yr <- raster::extract(h10[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi5 = as.data.frame(rbind(a, b, c))
msi5 <- msi5[,c(1,3)]
colnames(msi5) = c("msi", "parcela")
msi5 = msi5 %>% 
  mutate(data = "2010")
msi5_md = msi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[5]], area1[3,]); b3yr <- raster::extract(h11[[5]], area1[1,]); b1yr <- raster::extract(h11[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi6 = as.data.frame(rbind(a, b, c))
msi6 <- msi6[,c(1,3)]
colnames(msi6) = c("msi", "parcela")
msi6 = msi6 %>% 
  mutate(data = "2011")
msi6_md = msi6 %>%
  group_by(parcela) %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[5]], area1[3,]); b3yr <- raster::extract(h12[[5]], area1[1,]); b1yr <- raster::extract(h12[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
msi7 = as.data.frame(rbind(a, b, c))
msi7 <- msi7[,c(1,3)]
colnames(msi7) = c("msi", "parcela")
msi7 = msi7 %>% 
  mutate(data = "2012")
msi7_md = msi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(msi = median(msi)) %>% 
  mutate(data = "2012")


msi = as.data.frame(rbind(msi1, msi2, msi3, msi4, msi5, msi6, msi7))
msi_md = as.data.frame(rbind(msi1_md, msi2_md, msi3_md, msi4_md, msi5_md, msi6_md, msi7_md))



#Boxplot
ggplot(msi, aes(data,msi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="MSI")+
  theme_minimal()

#Medians
ggplot(msi_md, aes(data,msi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="MSI")+
  theme_minimal()

diff = as.data.frame(cbind(msi1_md, msi2_md, msi3_md, msi4_md, msi5_md, msi6_md, msi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no MSI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()