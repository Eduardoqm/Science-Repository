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
crt <- raster::extract(h04[[4]], area1[3,]); b3yr <- raster::extract(h04[[4]], area1[1,]); b1yr <- raster::extract(h04[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi21 = as.data.frame(rbind(a, b, c))
lwvi21 <- lwvi21[,c(1,3)]
colnames(lwvi21) = c("lwvi2", "parcela")
lwvi21 = lwvi21 %>% 
  mutate(data = "2004")
lwvi21_md = lwvi21 %>%
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[4]], area1[3,]); b3yr <- raster::extract(h05[[4]], area1[1,]); b1yr <- raster::extract(h05[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi22 = as.data.frame(rbind(a, b, c))
lwvi22 <- lwvi22[,c(1,3)]
colnames(lwvi22) = c("lwvi2", "parcela")
lwvi22 = lwvi22 %>% 
  mutate(data = "2005")
lwvi22_md = lwvi22 %>%
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[4]], area1[3,]); b3yr <- raster::extract(h06[[4]], area1[1,]); b1yr <- raster::extract(h06[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi23 = as.data.frame(rbind(a, b, c))
lwvi23 <- lwvi23[,c(1,3)]
colnames(lwvi23) = c("lwvi2", "parcela")
lwvi23 = lwvi23 %>% 
  mutate(data = "2006")
lwvi23_md = lwvi23 %>%
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[4]], area1[3,]); b3yr <- raster::extract(h08[[4]], area1[1,]); b1yr <- raster::extract(h08[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi24 = as.data.frame(rbind(a, b, c))
lwvi24 <- lwvi24[,c(1,3)]
colnames(lwvi24) = c("lwvi2", "parcela")
lwvi24 = lwvi24 %>% 
  mutate(data = "2008")
lwvi24_md = lwvi24 %>%
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[4]], area1[3,]); b3yr <- raster::extract(h10[[4]], area1[1,]); b1yr <- raster::extract(h10[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi25 = as.data.frame(rbind(a, b, c))
lwvi25 <- lwvi25[,c(1,3)]
colnames(lwvi25) = c("lwvi2", "parcela")
lwvi25 = lwvi25 %>% 
  mutate(data = "2010")
lwvi25_md = lwvi25 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[4]], area1[3,]); b3yr <- raster::extract(h11[[4]], area1[1,]); b1yr <- raster::extract(h11[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi26 = as.data.frame(rbind(a, b, c))
lwvi26 <- lwvi26[,c(1,3)]
colnames(lwvi26) = c("lwvi2", "parcela")
lwvi26 = lwvi26 %>% 
  mutate(data = "2011")
lwvi26_md = lwvi26 %>%
  group_by(parcela) %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[4]], area1[3,]); b3yr <- raster::extract(h12[[4]], area1[1,]); b1yr <- raster::extract(h12[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
lwvi27 = as.data.frame(rbind(a, b, c))
lwvi27 <- lwvi27[,c(1,3)]
colnames(lwvi27) = c("lwvi2", "parcela")
lwvi27 = lwvi27 %>% 
  mutate(data = "2012")
lwvi27_md = lwvi27 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(lwvi2 = median(lwvi2)) %>% 
  mutate(data = "2012")


lwvi2 = as.data.frame(rbind(lwvi21, lwvi22, lwvi23, lwvi24, lwvi25, lwvi26, lwvi27))
lwvi2_md = as.data.frame(rbind(lwvi21_md, lwvi22_md, lwvi23_md, lwvi24_md, lwvi25_md, lwvi26_md, lwvi27_md))



#Boxplot
ggplot(lwvi2, aes(data,lwvi2, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="LWVI2")+
  theme_minimal()

#Medians
ggplot(lwvi2_md, aes(data,lwvi2, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="LWVI2")+
  theme_minimal()

diff = as.data.frame(cbind(lwvi21_md, lwvi22_md, lwvi23_md, lwvi24_md, lwvi25_md, lwvi26_md, lwvi27_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no LWVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()