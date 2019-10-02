#Extract information from rasterized Area-1 data
#For all plot extention
# Eduardo Q Marques  02/10/2019

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank ===========
#Liteira
litt <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados rasterizados/Liteira", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")
area1 = spTransform(area1, crs(litt))

#Liteira ===================================================================
crt <- raster::extract(h04[[2]], area1[3,]); b3yr <- raster::extract(h04[[2]], area1[1,]); b1yr <- raster::extract(h04[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari1 = as.data.frame(rbind(a, b, c))
ari1 <- ari1[,c(1,3)]
colnames(ari1) = c("ari", "parcela")
ari1 = ari1 %>% 
  mutate(data = "2004")
ari1_md = ari1 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[2]], area1[3,]); b3yr <- raster::extract(h05[[2]], area1[1,]); b1yr <- raster::extract(h05[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari2 = as.data.frame(rbind(a, b, c))
ari2 <- ari2[,c(1,3)]
colnames(ari2) = c("ari", "parcela")
ari2 = ari2 %>% 
  mutate(data = "2005")
ari2_md = ari2 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[2]], area1[3,]); b3yr <- raster::extract(h06[[2]], area1[1,]); b1yr <- raster::extract(h06[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari3 = as.data.frame(rbind(a, b, c))
ari3 <- ari3[,c(1,3)]
colnames(ari3) = c("ari", "parcela")
ari3 = ari3 %>% 
  mutate(data = "2006")
ari3_md = ari3 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[2]], area1[3,]); b3yr <- raster::extract(h08[[2]], area1[1,]); b1yr <- raster::extract(h08[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari4 = as.data.frame(rbind(a, b, c))
ari4 <- ari4[,c(1,3)]
colnames(ari4) = c("ari", "parcela")
ari4 = ari4 %>% 
  mutate(data = "2008")
ari4_md = ari4 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[2]], area1[3,]); b3yr <- raster::extract(h10[[2]], area1[1,]); b1yr <- raster::extract(h10[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari5 = as.data.frame(rbind(a, b, c))
ari5 <- ari5[,c(1,3)]
colnames(ari5) = c("ari", "parcela")
ari5 = ari5 %>% 
  mutate(data = "2010")
ari5_md = ari5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[2]], area1[3,]); b3yr <- raster::extract(h11[[2]], area1[1,]); b1yr <- raster::extract(h11[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari6 = as.data.frame(rbind(a, b, c))
ari6 <- ari6[,c(1,3)]
colnames(ari6) = c("ari", "parcela")
ari6 = ari6 %>% 
  mutate(data = "2011")
ari6_md = ari6 %>%
  group_by(parcela) %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[2]], area1[3,]); b3yr <- raster::extract(h12[[2]], area1[1,]); b1yr <- raster::extract(h12[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ari7 = as.data.frame(rbind(a, b, c))
ari7 <- ari7[,c(1,3)]
colnames(ari7) = c("ari", "parcela")
ari7 = ari7 %>% 
  mutate(data = "2012")
ari7_md = ari7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ari = median(ari)) %>% 
  mutate(data = "2012")


ari = as.data.frame(rbind(ari1, ari2, ari3, ari4, ari5, ari6, ari7))
ari_md = as.data.frame(rbind(ari1_md, ari2_md, ari3_md, ari4_md, ari5_md, ari6_md, ari7_md))

#Boxplot
ggplot(ari, aes(data,ari, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="ARI")+
  theme_minimal()

#Medians
ggplot(ari_md, aes(data,ari, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="ARI")+
  theme_minimal()

#Extract difference
diff = as.data.frame(cbind(ari1_md, ari2_md, ari3_md, ari4_md, ari5_md, ari6_md, ari7_md))
diff = diff[,c(1,2,5,8,11,14,17,20)]
colnames(diff) = c("parcela","2004","2005","2006","2008","2010","2011","2012")
diff <- as.data.frame(t(diff))
colnames(diff) = c("b1yr","b3yr","controle")
diff <- diff[-c(1), ]

diff$b3yr = as.numeric(as.character(diff$b3yr))
diff$b1yr = as.numeric(as.character(diff$b1yr))
diff$controle = as.numeric(as.character(diff$controle))

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no ARI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()
