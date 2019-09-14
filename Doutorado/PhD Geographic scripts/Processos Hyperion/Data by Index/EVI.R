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
crt <- raster::extract(h04[[2]], area1[3,]); b3yr <- raster::extract(h04[[2]], area1[1,]); b1yr <- raster::extract(h04[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi1 = as.data.frame(rbind(a, b, c))
evi1 <- evi1[,c(1,3)]
colnames(evi1) = c("evi", "parcela")
evi1 = evi1 %>% 
  mutate(data = "2004")
evi1_md = evi1 %>%
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[2]], area1[3,]); b3yr <- raster::extract(h05[[2]], area1[1,]); b1yr <- raster::extract(h05[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi2 = as.data.frame(rbind(a, b, c))
evi2 <- evi2[,c(1,3)]
colnames(evi2) = c("evi", "parcela")
evi2 = evi2 %>% 
  mutate(data = "2005")
evi2_md = evi2 %>%
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[2]], area1[3,]); b3yr <- raster::extract(h06[[2]], area1[1,]); b1yr <- raster::extract(h06[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi3 = as.data.frame(rbind(a, b, c))
evi3 <- evi3[,c(1,3)]
colnames(evi3) = c("evi", "parcela")
evi3 = evi3 %>% 
  mutate(data = "2006")
evi3_md = evi3 %>%
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[2]], area1[3,]); b3yr <- raster::extract(h08[[2]], area1[1,]); b1yr <- raster::extract(h08[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi4 = as.data.frame(rbind(a, b, c))
evi4 <- evi4[,c(1,3)]
colnames(evi4) = c("evi", "parcela")
evi4 = evi4 %>% 
  mutate(data = "2008")
evi4_md = evi4 %>%
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[2]], area1[3,]); b3yr <- raster::extract(h10[[2]], area1[1,]); b1yr <- raster::extract(h10[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi5 = as.data.frame(rbind(a, b, c))
evi5 <- evi5[,c(1,3)]
colnames(evi5) = c("evi", "parcela")
evi5 = evi5 %>% 
  mutate(data = "2010")
evi5_md = evi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[2]], area1[3,]); b3yr <- raster::extract(h11[[2]], area1[1,]); b1yr <- raster::extract(h11[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi6 = as.data.frame(rbind(a, b, c))
evi6 <- evi6[,c(1,3)]
colnames(evi6) = c("evi", "parcela")
evi6 = evi6 %>% 
  mutate(data = "2011")
evi6_md = evi6 %>%
  group_by(parcela) %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[2]], area1[3,]); b3yr <- raster::extract(h12[[2]], area1[1,]); b1yr <- raster::extract(h12[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
evi7 = as.data.frame(rbind(a, b, c))
evi7 <- evi7[,c(1,3)]
colnames(evi7) = c("evi", "parcela")
evi7 = evi7 %>% 
  mutate(data = "2012")
evi7_md = evi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(evi = median(evi)) %>% 
  mutate(data = "2012")


evi = as.data.frame(rbind(evi1, evi2, evi3, evi4, evi5, evi6, evi7))
evi_md = as.data.frame(rbind(evi1_md, evi2_md, evi3_md, evi4_md, evi5_md, evi6_md, evi7_md))



#Boxplot
ggplot(evi, aes(data,evi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="EVI")+
  theme_minimal()

#Medians
ggplot(evi_md, aes(data,evi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="EVI")+
  theme_minimal()

diff = as.data.frame(cbind(evi1_md, evi2_md, evi3_md, evi4_md, evi5_md, evi6_md, evi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no EVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()