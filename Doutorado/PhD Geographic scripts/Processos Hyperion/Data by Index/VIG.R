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
crt <- raster::extract(h04[[16]], area1[3,]); b3yr <- raster::extract(h04[[16]], area1[1,]); b1yr <- raster::extract(h04[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig1 = as.data.frame(rbind(a, b, c))
vig1 <- vig1[,c(1,3)]
colnames(vig1) = c("vig", "parcela")
vig1 = vig1 %>% 
  mutate(data = "2004")
vig1_md = vig1 %>%
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[16]], area1[3,]); b3yr <- raster::extract(h05[[16]], area1[1,]); b1yr <- raster::extract(h05[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig2 = as.data.frame(rbind(a, b, c))
vig2 <- vig2[,c(1,3)]
colnames(vig2) = c("vig", "parcela")
vig2 = vig2 %>% 
  mutate(data = "2005")
vig2_md = vig2 %>%
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[16]], area1[3,]); b3yr <- raster::extract(h06[[16]], area1[1,]); b1yr <- raster::extract(h06[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig3 = as.data.frame(rbind(a, b, c))
vig3 <- vig3[,c(1,3)]
colnames(vig3) = c("vig", "parcela")
vig3 = vig3 %>% 
  mutate(data = "2006")
vig3_md = vig3 %>%
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[16]], area1[3,]); b3yr <- raster::extract(h08[[16]], area1[1,]); b1yr <- raster::extract(h08[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig4 = as.data.frame(rbind(a, b, c))
vig4 <- vig4[,c(1,3)]
colnames(vig4) = c("vig", "parcela")
vig4 = vig4 %>% 
  mutate(data = "2008")
vig4_md = vig4 %>%
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[16]], area1[3,]); b3yr <- raster::extract(h10[[16]], area1[1,]); b1yr <- raster::extract(h10[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig5 = as.data.frame(rbind(a, b, c))
vig5 <- vig5[,c(1,3)]
colnames(vig5) = c("vig", "parcela")
vig5 = vig5 %>% 
  mutate(data = "2010")
vig5_md = vig5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[16]], area1[3,]); b3yr <- raster::extract(h11[[16]], area1[1,]); b1yr <- raster::extract(h11[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig6 = as.data.frame(rbind(a, b, c))
vig6 <- vig6[,c(1,3)]
colnames(vig6) = c("vig", "parcela")
vig6 = vig6 %>% 
  mutate(data = "2011")
vig6_md = vig6 %>%
  group_by(parcela) %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[16]], area1[3,]); b3yr <- raster::extract(h12[[16]], area1[1,]); b1yr <- raster::extract(h12[[16]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vig7 = as.data.frame(rbind(a, b, c))
vig7 <- vig7[,c(1,3)]
colnames(vig7) = c("vig", "parcela")
vig7 = vig7 %>% 
  mutate(data = "2012")
vig7_md = vig7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(vig = median(vig)) %>% 
  mutate(data = "2012")


vig = as.data.frame(rbind(vig1, vig2, vig3, vig4, vig5, vig6, vig7))
vig_md = as.data.frame(rbind(vig1_md, vig2_md, vig3_md, vig4_md, vig5_md, vig6_md, vig7_md))



#Boxplot
ggplot(vig, aes(data,vig, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="VIG")+
  theme_minimal()

#Medians
ggplot(vig_md, aes(data,vig, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="VIG")+
  theme_minimal()

diff = as.data.frame(cbind(vig1_md, vig2_md, vig3_md, vig4_md, vig5_md, vig6_md, vig7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no VIG)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()