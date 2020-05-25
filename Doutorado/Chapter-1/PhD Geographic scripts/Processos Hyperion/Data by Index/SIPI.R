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
crt <- raster::extract(h04[[14]], area1[3,]); b3yr <- raster::extract(h04[[14]], area1[1,]); b1yr <- raster::extract(h04[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi1 = as.data.frame(rbind(a, b, c))
sipi1 <- sipi1[,c(1,3)]
colnames(sipi1) = c("sipi", "parcela")
sipi1 = sipi1 %>% 
  mutate(data = "2004")
sipi1_md = sipi1 %>%
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[14]], area1[3,]); b3yr <- raster::extract(h05[[14]], area1[1,]); b1yr <- raster::extract(h05[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi2 = as.data.frame(rbind(a, b, c))
sipi2 <- sipi2[,c(1,3)]
colnames(sipi2) = c("sipi", "parcela")
sipi2 = sipi2 %>% 
  mutate(data = "2005")
sipi2_md = sipi2 %>%
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[14]], area1[3,]); b3yr <- raster::extract(h06[[14]], area1[1,]); b1yr <- raster::extract(h06[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi3 = as.data.frame(rbind(a, b, c))
sipi3 <- sipi3[,c(1,3)]
colnames(sipi3) = c("sipi", "parcela")
sipi3 = sipi3 %>% 
  mutate(data = "2006")
sipi3_md = sipi3 %>%
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[14]], area1[3,]); b3yr <- raster::extract(h08[[14]], area1[1,]); b1yr <- raster::extract(h08[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi4 = as.data.frame(rbind(a, b, c))
sipi4 <- sipi4[,c(1,3)]
colnames(sipi4) = c("sipi", "parcela")
sipi4 = sipi4 %>% 
  mutate(data = "2008")
sipi4_md = sipi4 %>%
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[14]], area1[3,]); b3yr <- raster::extract(h10[[14]], area1[1,]); b1yr <- raster::extract(h10[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi5 = as.data.frame(rbind(a, b, c))
sipi5 <- sipi5[,c(1,3)]
colnames(sipi5) = c("sipi", "parcela")
sipi5 = sipi5 %>% 
  mutate(data = "2010")
sipi5_md = sipi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[14]], area1[3,]); b3yr <- raster::extract(h11[[14]], area1[1,]); b1yr <- raster::extract(h11[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi6 = as.data.frame(rbind(a, b, c))
sipi6 <- sipi6[,c(1,3)]
colnames(sipi6) = c("sipi", "parcela")
sipi6 = sipi6 %>% 
  mutate(data = "2011")
sipi6_md = sipi6 %>%
  group_by(parcela) %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[14]], area1[3,]); b3yr <- raster::extract(h12[[14]], area1[1,]); b1yr <- raster::extract(h12[[14]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
sipi7 = as.data.frame(rbind(a, b, c))
sipi7 <- sipi7[,c(1,3)]
colnames(sipi7) = c("sipi", "parcela")
sipi7 = sipi7 %>% 
  mutate(data = "2012")
sipi7_md = sipi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(sipi = median(sipi)) %>% 
  mutate(data = "2012")


sipi = as.data.frame(rbind(sipi1, sipi2, sipi3, sipi4, sipi5, sipi6, sipi7))
sipi_md = as.data.frame(rbind(sipi1_md, sipi2_md, sipi3_md, sipi4_md, sipi5_md, sipi6_md, sipi7_md))



#Boxplot
ggplot(sipi, aes(data,sipi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="SIPI")+
  theme_minimal()

#Medians
ggplot(sipi_md, aes(data,sipi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="SIPI")+
  theme_minimal()

diff = as.data.frame(cbind(sipi1_md, sipi2_md, sipi3_md, sipi4_md, sipi5_md, sipi6_md, sipi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no SIPI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()