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
crt <- raster::extract(h04[[18]], area1[3,]); b3yr <- raster::extract(h04[[18]], area1[1,]); b1yr <- raster::extract(h04[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri21 = as.data.frame(rbind(a, b, c))
nbri21 <- nbri21[,c(1,3)]
colnames(nbri21) = c("nbri2", "parcela")
nbri21 = nbri21 %>% 
  mutate(data = "2004")
nbri21_md = nbri21 %>%
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[18]], area1[3,]); b3yr <- raster::extract(h05[[18]], area1[1,]); b1yr <- raster::extract(h05[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri22 = as.data.frame(rbind(a, b, c))
nbri22 <- nbri22[,c(1,3)]
colnames(nbri22) = c("nbri2", "parcela")
nbri22 = nbri22 %>% 
  mutate(data = "2005")
nbri22_md = nbri22 %>%
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[18]], area1[3,]); b3yr <- raster::extract(h06[[18]], area1[1,]); b1yr <- raster::extract(h06[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri23 = as.data.frame(rbind(a, b, c))
nbri23 <- nbri23[,c(1,3)]
colnames(nbri23) = c("nbri2", "parcela")
nbri23 = nbri23 %>% 
  mutate(data = "2006")
nbri23_md = nbri23 %>%
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[18]], area1[3,]); b3yr <- raster::extract(h08[[18]], area1[1,]); b1yr <- raster::extract(h08[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri24 = as.data.frame(rbind(a, b, c))
nbri24 <- nbri24[,c(1,3)]
colnames(nbri24) = c("nbri2", "parcela")
nbri24 = nbri24 %>% 
  mutate(data = "2008")
nbri24_md = nbri24 %>%
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[18]], area1[3,]); b3yr <- raster::extract(h10[[18]], area1[1,]); b1yr <- raster::extract(h10[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri25 = as.data.frame(rbind(a, b, c))
nbri25 <- nbri25[,c(1,3)]
colnames(nbri25) = c("nbri2", "parcela")
nbri25 = nbri25 %>% 
  mutate(data = "2010")
nbri25_md = nbri25 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[18]], area1[3,]); b3yr <- raster::extract(h11[[18]], area1[1,]); b1yr <- raster::extract(h11[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri26 = as.data.frame(rbind(a, b, c))
nbri26 <- nbri26[,c(1,3)]
colnames(nbri26) = c("nbri2", "parcela")
nbri26 = nbri26 %>% 
  mutate(data = "2011")
nbri26_md = nbri26 %>%
  group_by(parcela) %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[18]], area1[3,]); b3yr <- raster::extract(h12[[18]], area1[1,]); b1yr <- raster::extract(h12[[18]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nbri27 = as.data.frame(rbind(a, b, c))
nbri27 <- nbri27[,c(1,3)]
colnames(nbri27) = c("nbri2", "parcela")
nbri27 = nbri27 %>% 
  mutate(data = "2012")
nbri27_md = nbri27 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(nbri2 = median(nbri2)) %>% 
  mutate(data = "2012")


nbri2 = as.data.frame(rbind(nbri21, nbri22, nbri23, nbri24, nbri25, nbri26, nbri27))
nbri2_md = as.data.frame(rbind(nbri21_md, nbri22_md, nbri23_md, nbri24_md, nbri25_md, nbri26_md, nbri27_md))



#Boxplot
ggplot(nbri2, aes(data,nbri2, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NBRI2")+
  theme_minimal()

#Medians
ggplot(nbri2_md, aes(data,nbri2, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NBRI2")+
  theme_minimal()

diff = as.data.frame(cbind(nbri21_md, nbri22_md, nbri23_md, nbri24_md, nbri25_md, nbri26_md, nbri27_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no NBRI2)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()