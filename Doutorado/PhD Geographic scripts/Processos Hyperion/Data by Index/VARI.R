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
crt <- raster::extract(h04[[15]], area1[3,]); b3yr <- raster::extract(h04[[15]], area1[1,]); b1yr <- raster::extract(h04[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari1 = as.data.frame(rbind(a, b, c))
vari1 <- vari1[,c(1,3)]
colnames(vari1) = c("vari", "parcela")
vari1 = vari1 %>% 
  mutate(data = "2004")
vari1_md = vari1 %>%
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[15]], area1[3,]); b3yr <- raster::extract(h05[[15]], area1[1,]); b1yr <- raster::extract(h05[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari2 = as.data.frame(rbind(a, b, c))
vari2 <- vari2[,c(1,3)]
colnames(vari2) = c("vari", "parcela")
vari2 = vari2 %>% 
  mutate(data = "2005")
vari2_md = vari2 %>%
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[15]], area1[3,]); b3yr <- raster::extract(h06[[15]], area1[1,]); b1yr <- raster::extract(h06[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari3 = as.data.frame(rbind(a, b, c))
vari3 <- vari3[,c(1,3)]
colnames(vari3) = c("vari", "parcela")
vari3 = vari3 %>% 
  mutate(data = "2006")
vari3_md = vari3 %>%
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[15]], area1[3,]); b3yr <- raster::extract(h08[[15]], area1[1,]); b1yr <- raster::extract(h08[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari4 = as.data.frame(rbind(a, b, c))
vari4 <- vari4[,c(1,3)]
colnames(vari4) = c("vari", "parcela")
vari4 = vari4 %>% 
  mutate(data = "2008")
vari4_md = vari4 %>%
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[15]], area1[3,]); b3yr <- raster::extract(h10[[15]], area1[1,]); b1yr <- raster::extract(h10[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari5 = as.data.frame(rbind(a, b, c))
vari5 <- vari5[,c(1,3)]
colnames(vari5) = c("vari", "parcela")
vari5 = vari5 %>% 
  mutate(data = "2010")
vari5_md = vari5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[15]], area1[3,]); b3yr <- raster::extract(h11[[15]], area1[1,]); b1yr <- raster::extract(h11[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari6 = as.data.frame(rbind(a, b, c))
vari6 <- vari6[,c(1,3)]
colnames(vari6) = c("vari", "parcela")
vari6 = vari6 %>% 
  mutate(data = "2011")
vari6_md = vari6 %>%
  group_by(parcela) %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[15]], area1[3,]); b3yr <- raster::extract(h12[[15]], area1[1,]); b1yr <- raster::extract(h12[[15]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
vari7 = as.data.frame(rbind(a, b, c))
vari7 <- vari7[,c(1,3)]
colnames(vari7) = c("vari", "parcela")
vari7 = vari7 %>% 
  mutate(data = "2012")
vari7_md = vari7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(vari = median(vari)) %>% 
  mutate(data = "2012")


vari = as.data.frame(rbind(vari1, vari2, vari3, vari4, vari5, vari6, vari7))
vari_md = as.data.frame(rbind(vari1_md, vari2_md, vari3_md, vari4_md, vari5_md, vari6_md, vari7_md))



#Boxplot
ggplot(vari, aes(data,vari, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0)+
  labs(fill= "Plot",x="Ano",y="VARI")+
  theme_minimal()

#Medians
ggplot(vari_md, aes(data,vari, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="VARI")+
  theme_minimal()

diff = as.data.frame(cbind(vari1_md, vari2_md, vari3_md, vari4_md, vari5_md, vari6_md, vari7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no VARI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()