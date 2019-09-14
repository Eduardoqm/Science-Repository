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
crt <- raster::extract(h04[[17]], area1[3,]); b3yr <- raster::extract(h04[[17]], area1[1,]); b1yr <- raster::extract(h04[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi1 = as.data.frame(rbind(a, b, c))
wbi1 <- wbi1[,c(1,3)]
colnames(wbi1) = c("wbi", "parcela")
wbi1 = wbi1 %>% 
  mutate(data = "2004")
wbi1_md = wbi1 %>%
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[17]], area1[3,]); b3yr <- raster::extract(h05[[17]], area1[1,]); b1yr <- raster::extract(h05[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi2 = as.data.frame(rbind(a, b, c))
wbi2 <- wbi2[,c(1,3)]
colnames(wbi2) = c("wbi", "parcela")
wbi2 = wbi2 %>% 
  mutate(data = "2005")
wbi2_md = wbi2 %>%
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[17]], area1[3,]); b3yr <- raster::extract(h06[[17]], area1[1,]); b1yr <- raster::extract(h06[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi3 = as.data.frame(rbind(a, b, c))
wbi3 <- wbi3[,c(1,3)]
colnames(wbi3) = c("wbi", "parcela")
wbi3 = wbi3 %>% 
  mutate(data = "2006")
wbi3_md = wbi3 %>%
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[17]], area1[3,]); b3yr <- raster::extract(h08[[17]], area1[1,]); b1yr <- raster::extract(h08[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi4 = as.data.frame(rbind(a, b, c))
wbi4 <- wbi4[,c(1,3)]
colnames(wbi4) = c("wbi", "parcela")
wbi4 = wbi4 %>% 
  mutate(data = "2008")
wbi4_md = wbi4 %>%
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[17]], area1[3,]); b3yr <- raster::extract(h10[[17]], area1[1,]); b1yr <- raster::extract(h10[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi5 = as.data.frame(rbind(a, b, c))
wbi5 <- wbi5[,c(1,3)]
colnames(wbi5) = c("wbi", "parcela")
wbi5 = wbi5 %>% 
  mutate(data = "2010")
wbi5_md = wbi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[17]], area1[3,]); b3yr <- raster::extract(h11[[17]], area1[1,]); b1yr <- raster::extract(h11[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi6 = as.data.frame(rbind(a, b, c))
wbi6 <- wbi6[,c(1,3)]
colnames(wbi6) = c("wbi", "parcela")
wbi6 = wbi6 %>% 
  mutate(data = "2011")
wbi6_md = wbi6 %>%
  group_by(parcela) %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[17]], area1[3,]); b3yr <- raster::extract(h12[[17]], area1[1,]); b1yr <- raster::extract(h12[[17]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
wbi7 = as.data.frame(rbind(a, b, c))
wbi7 <- wbi7[,c(1,3)]
colnames(wbi7) = c("wbi", "parcela")
wbi7 = wbi7 %>% 
  mutate(data = "2012")
wbi7_md = wbi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(wbi = median(wbi)) %>% 
  mutate(data = "2012")


wbi = as.data.frame(rbind(wbi1, wbi2, wbi3, wbi4, wbi5, wbi6, wbi7))
wbi_md = as.data.frame(rbind(wbi1_md, wbi2_md, wbi3_md, wbi4_md, wbi5_md, wbi6_md, wbi7_md))



#Boxplot
ggplot(wbi, aes(data,wbi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.0)+
  labs(fill= "Plot",x="Ano",y="WBI")+
  theme_minimal()

#Medians
ggplot(wbi_md, aes(data,wbi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="WBI")+
  theme_minimal()

diff = as.data.frame(cbind(wbi1_md, wbi2_md, wbi3_md, wbi4_md, wbi5_md, wbi6_md, wbi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no WBI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()