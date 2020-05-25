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
crt <- raster::extract(h04[[12]], area1[3,]); b3yr <- raster::extract(h04[[12]], area1[1,]); b1yr <- raster::extract(h04[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr1 = as.data.frame(rbind(a, b, c))
pssr1 <- pssr1[,c(1,3)]
colnames(pssr1) = c("pssr", "parcela")
pssr1 = pssr1 %>% 
  mutate(data = "2004")
pssr1_md = pssr1 %>%
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[12]], area1[3,]); b3yr <- raster::extract(h05[[12]], area1[1,]); b1yr <- raster::extract(h05[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr2 = as.data.frame(rbind(a, b, c))
pssr2 <- pssr2[,c(1,3)]
colnames(pssr2) = c("pssr", "parcela")
pssr2 = pssr2 %>% 
  mutate(data = "2005")
pssr2_md = pssr2 %>%
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[12]], area1[3,]); b3yr <- raster::extract(h06[[12]], area1[1,]); b1yr <- raster::extract(h06[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr3 = as.data.frame(rbind(a, b, c))
pssr3 <- pssr3[,c(1,3)]
colnames(pssr3) = c("pssr", "parcela")
pssr3 = pssr3 %>% 
  mutate(data = "2006")
pssr3_md = pssr3 %>%
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[12]], area1[3,]); b3yr <- raster::extract(h08[[12]], area1[1,]); b1yr <- raster::extract(h08[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr4 = as.data.frame(rbind(a, b, c))
pssr4 <- pssr4[,c(1,3)]
colnames(pssr4) = c("pssr", "parcela")
pssr4 = pssr4 %>% 
  mutate(data = "2008")
pssr4_md = pssr4 %>%
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[12]], area1[3,]); b3yr <- raster::extract(h10[[12]], area1[1,]); b1yr <- raster::extract(h10[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr5 = as.data.frame(rbind(a, b, c))
pssr5 <- pssr5[,c(1,3)]
colnames(pssr5) = c("pssr", "parcela")
pssr5 = pssr5 %>% 
  mutate(data = "2010")
pssr5_md = pssr5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[12]], area1[3,]); b3yr <- raster::extract(h11[[12]], area1[1,]); b1yr <- raster::extract(h11[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr6 = as.data.frame(rbind(a, b, c))
pssr6 <- pssr6[,c(1,3)]
colnames(pssr6) = c("pssr", "parcela")
pssr6 = pssr6 %>% 
  mutate(data = "2011")
pssr6_md = pssr6 %>%
  group_by(parcela) %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[12]], area1[3,]); b3yr <- raster::extract(h12[[12]], area1[1,]); b1yr <- raster::extract(h12[[12]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pssr7 = as.data.frame(rbind(a, b, c))
pssr7 <- pssr7[,c(1,3)]
colnames(pssr7) = c("pssr", "parcela")
pssr7 = pssr7 %>% 
  mutate(data = "2012")
pssr7_md = pssr7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(pssr = median(pssr)) %>% 
  mutate(data = "2012")


pssr = as.data.frame(rbind(pssr1, pssr2, pssr3, pssr4, pssr5, pssr6, pssr7))
pssr_md = as.data.frame(rbind(pssr1_md, pssr2_md, pssr3_md, pssr4_md, pssr5_md, pssr6_md, pssr7_md))



#Boxplot
ggplot(pssr, aes(data,pssr, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="PSSR")+
  theme_minimal()

#Medians
ggplot(pssr_md, aes(data,pssr, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="PSSR")+
  theme_minimal()

diff = as.data.frame(cbind(pssr1_md, pssr2_md, pssr3_md, pssr4_md, pssr5_md, pssr6_md, pssr7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no PSSR)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()