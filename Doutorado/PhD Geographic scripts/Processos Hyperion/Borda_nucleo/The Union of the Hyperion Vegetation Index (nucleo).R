###############################################
# THE UNION OF THE HYPERION VEGETATION INDEXS #
#          => A Temporal scale <==            #
#            => Edge and Core <=              #
# By: Eduardo Q Marques   03-12-2019          #
###############################################

#OBS: Same years are cutted control. So I change control area to other place that the forest is preserved
library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(viridis)

#Data bank ===========================================================================
h04 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2004", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h05 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2005", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h06 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2006", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h08 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2008", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h10 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2010", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h11 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

h12 <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2012", pattern = ".tif$", full.names=TRUE,recursive=TRUE))

#Polygon to get values
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Hyperion",layer="Borda_nucleo_hyperion")
area1 = spTransform(area1, crs(h04))

#ARI ===================================================================
crt <- raster::extract(h04[[2]], area1[1,]); b3yr <- raster::extract(h04[[2]], area1[3,]); b1yr <- raster::extract(h04[[2]], area1[5,])
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

crt <- raster::extract(h05[[2]], area1[1,]); b3yr <- raster::extract(h05[[2]], area1[3,]); b1yr <- raster::extract(h05[[2]], area1[5,])
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

crt <- raster::extract(h06[[2]], area1[1,]); b3yr <- raster::extract(h06[[2]], area1[3,]); b1yr <- raster::extract(h06[[2]], area1[5,])
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

crt <- raster::extract(h08[[2]], area1[1,]); b3yr <- raster::extract(h08[[2]], area1[3,]); b1yr <- raster::extract(h08[[2]], area1[5,])
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

crt <- raster::extract(h10[[2]], area1[1,]); b3yr <- raster::extract(h10[[2]], area1[3,]); b1yr <- raster::extract(h10[[2]], area1[5,])
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

crt <- raster::extract(h11[[2]], area1[1,]); b3yr <- raster::extract(h11[[2]], area1[3,]); b1yr <- raster::extract(h11[[2]], area1[5,])
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

crt <- raster::extract(h12[[2]], area1[1,]); b3yr <- raster::extract(h12[[2]], area1[3,]); b1yr <- raster::extract(h12[[2]], area1[5,])
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

#Creat data to plot all index (diferrence %)
b1yr_1=gg[-c(8:14), ]
b1yr_1$indice = c("ARI")

b3yr_1=gg[-c(1:7), ]
b3yr_1$indice = c("ARI")

#EVI ===================================================================
crt <- raster::extract(h04[[2]], area1[1,]); b3yr <- raster::extract(h04[[2]], area1[3,]); b1yr <- raster::extract(h04[[2]], area1[5,])
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

crt <- raster::extract(h05[[2]], area1[1,]); b3yr <- raster::extract(h05[[2]], area1[3,]); b1yr <- raster::extract(h05[[2]], area1[5,])
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

crt <- raster::extract(h06[[2]], area1[1,]); b3yr <- raster::extract(h06[[2]], area1[3,]); b1yr <- raster::extract(h06[[2]], area1[5,])
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

crt <- raster::extract(h08[[2]], area1[1,]); b3yr <- raster::extract(h08[[2]], area1[3,]); b1yr <- raster::extract(h08[[2]], area1[5,])
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

crt <- raster::extract(h10[[2]], area1[1,]); b3yr <- raster::extract(h10[[2]], area1[3,]); b1yr <- raster::extract(h10[[2]], area1[5,])
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

crt <- raster::extract(h11[[2]], area1[1,]); b3yr <- raster::extract(h11[[2]], area1[3,]); b1yr <- raster::extract(h11[[2]], area1[5,])
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

crt <- raster::extract(h12[[2]], area1[1,]); b3yr <- raster::extract(h12[[2]], area1[3,]); b1yr <- raster::extract(h12[[2]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="EVI")+
  theme_minimal()

#Medians
ggplot(evi_md, aes(data,evi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="EVI")+
  theme_minimal()

#Extract the difference
diff = as.data.frame(cbind(evi1_md, evi2_md, evi3_md, evi4_md, evi5_md, evi6_md, evi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no EVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_2=gg[-c(8:14), ]
b1yr_2$indice = c("EVI")

b3yr_2=gg[-c(1:7), ]
b3yr_2$indice = c("EVI")

#LWVI2 ===================================================================
crt <- raster::extract(h04[[4]], area1[1,]); b3yr <- raster::extract(h04[[4]], area1[3,]); b1yr <- raster::extract(h04[[4]], area1[5,])
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

crt <- raster::extract(h05[[4]], area1[1,]); b3yr <- raster::extract(h05[[4]], area1[3,]); b1yr <- raster::extract(h05[[4]], area1[5,])
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

crt <- raster::extract(h06[[4]], area1[1,]); b3yr <- raster::extract(h06[[4]], area1[3,]); b1yr <- raster::extract(h06[[4]], area1[5,])
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

crt <- raster::extract(h08[[4]], area1[1,]); b3yr <- raster::extract(h08[[4]], area1[3,]); b1yr <- raster::extract(h08[[4]], area1[5,])
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

crt <- raster::extract(h10[[4]], area1[1,]); b3yr <- raster::extract(h10[[4]], area1[3,]); b1yr <- raster::extract(h10[[4]], area1[5,])
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

crt <- raster::extract(h11[[4]], area1[1,]); b3yr <- raster::extract(h11[[4]], area1[3,]); b1yr <- raster::extract(h11[[4]], area1[5,])
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

crt <- raster::extract(h12[[4]], area1[1,]); b3yr <- raster::extract(h12[[4]], area1[3,]); b1yr <- raster::extract(h12[[4]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no LWVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_3=gg[-c(8:14), ]
b1yr_3$indice = c("LWVI")

b3yr_3=gg[-c(1:7), ]
b3yr_3$indice = c("LWVI")
#MSI ===================================================================
crt <- raster::extract(h04[[5]], area1[1,]); b3yr <- raster::extract(h04[[5]], area1[3,]); b1yr <- raster::extract(h04[[5]], area1[5,])
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

crt <- raster::extract(h05[[5]], area1[1,]); b3yr <- raster::extract(h05[[5]], area1[3,]); b1yr <- raster::extract(h05[[5]], area1[5,])
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

crt <- raster::extract(h06[[5]], area1[1,]); b3yr <- raster::extract(h06[[5]], area1[3,]); b1yr <- raster::extract(h06[[5]], area1[5,])
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

crt <- raster::extract(h08[[5]], area1[1,]); b3yr <- raster::extract(h08[[5]], area1[3,]); b1yr <- raster::extract(h08[[5]], area1[5,])
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

crt <- raster::extract(h10[[5]], area1[1,]); b3yr <- raster::extract(h10[[5]], area1[3,]); b1yr <- raster::extract(h10[[5]], area1[5,])
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

crt <- raster::extract(h11[[5]], area1[1,]); b3yr <- raster::extract(h11[[5]], area1[3,]); b1yr <- raster::extract(h11[[5]], area1[5,])
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

crt <- raster::extract(h12[[5]], area1[1,]); b3yr <- raster::extract(h12[[5]], area1[3,]); b1yr <- raster::extract(h12[[5]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no MSI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_4=gg[-c(8:14), ]
b1yr_4$indice = c("MSI")

b3yr_4=gg[-c(1:7), ]
b3yr_4$indice = c("MSI")
#NBRI ===================================================================
crt <- raster::extract(h04[[6]], area1[1,]); b3yr <- raster::extract(h04[[6]], area1[3,]); b1yr <- raster::extract(h04[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii1 = as.data.frame(rbind(a, b, c))
ndii1 <- ndii1[,c(1,3)]
colnames(ndii1) = c("ndii", "parcela")
ndii1 = ndii1 %>% 
  mutate(data = "2004")
ndii1_md = ndii1 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[6]], area1[1,]); b3yr <- raster::extract(h05[[6]], area1[3,]); b1yr <- raster::extract(h05[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii2 = as.data.frame(rbind(a, b, c))
ndii2 <- ndii2[,c(1,3)]
colnames(ndii2) = c("ndii", "parcela")
ndii2 = ndii2 %>% 
  mutate(data = "2005")
ndii2_md = ndii2 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[6]], area1[1,]); b3yr <- raster::extract(h06[[6]], area1[3,]); b1yr <- raster::extract(h06[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii3 = as.data.frame(rbind(a, b, c))
ndii3 <- ndii3[,c(1,3)]
colnames(ndii3) = c("ndii", "parcela")
ndii3 = ndii3 %>% 
  mutate(data = "2006")
ndii3_md = ndii3 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[6]], area1[1,]); b3yr <- raster::extract(h08[[6]], area1[3,]); b1yr <- raster::extract(h08[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii4 = as.data.frame(rbind(a, b, c))
ndii4 <- ndii4[,c(1,3)]
colnames(ndii4) = c("ndii", "parcela")
ndii4 = ndii4 %>% 
  mutate(data = "2008")
ndii4_md = ndii4 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[6]], area1[1,]); b3yr <- raster::extract(h10[[6]], area1[3,]); b1yr <- raster::extract(h10[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii5 = as.data.frame(rbind(a, b, c))
ndii5 <- ndii5[,c(1,3)]
colnames(ndii5) = c("ndii", "parcela")
ndii5 = ndii5 %>% 
  mutate(data = "2010")
ndii5_md = ndii5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[6]], area1[1,]); b3yr <- raster::extract(h11[[6]], area1[3,]); b1yr <- raster::extract(h11[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii6 = as.data.frame(rbind(a, b, c))
ndii6 <- ndii6[,c(1,3)]
colnames(ndii6) = c("ndii", "parcela")
ndii6 = ndii6 %>% 
  mutate(data = "2011")
ndii6_md = ndii6 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[6]], area1[1,]); b3yr <- raster::extract(h12[[6]], area1[3,]); b1yr <- raster::extract(h12[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii7 = as.data.frame(rbind(a, b, c))
ndii7 <- ndii7[,c(1,3)]
colnames(ndii7) = c("ndii", "parcela")
ndii7 = ndii7 %>% 
  mutate(data = "2012")
ndii7_md = ndii7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2012")


ndii = as.data.frame(rbind(ndii1, ndii2, ndii3, ndii4, ndii5, ndii6, ndii7))
ndii_md = as.data.frame(rbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))



#Boxplot
ggplot(ndii, aes(data,ndii, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NBRI")+
  theme_minimal()

#Medians
ggplot(ndii_md, aes(data,ndii, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NBRI")+
  theme_minimal()

diff = as.data.frame(cbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NBRI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_5=gg[-c(8:14), ]
b1yr_5$indice = c("NBRI")

b3yr_5=gg[-c(1:7), ]
b3yr_5$indice = c("NBRI")
#NBRI2 ===================================================================
crt <- raster::extract(h04[[18]], area1[1,]); b3yr <- raster::extract(h04[[18]], area1[3,]); b1yr <- raster::extract(h04[[18]], area1[5,])
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

crt <- raster::extract(h05[[18]], area1[1,]); b3yr <- raster::extract(h05[[18]], area1[3,]); b1yr <- raster::extract(h05[[18]], area1[5,])
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

crt <- raster::extract(h06[[18]], area1[1,]); b3yr <- raster::extract(h06[[18]], area1[3,]); b1yr <- raster::extract(h06[[18]], area1[5,])
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

crt <- raster::extract(h08[[18]], area1[1,]); b3yr <- raster::extract(h08[[18]], area1[3,]); b1yr <- raster::extract(h08[[18]], area1[5,])
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

crt <- raster::extract(h10[[18]], area1[1,]); b3yr <- raster::extract(h10[[18]], area1[3,]); b1yr <- raster::extract(h10[[18]], area1[5,])
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

crt <- raster::extract(h11[[18]], area1[1,]); b3yr <- raster::extract(h11[[18]], area1[3,]); b1yr <- raster::extract(h11[[18]], area1[5,])
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

crt <- raster::extract(h12[[18]], area1[1,]); b3yr <- raster::extract(h12[[18]], area1[3,]); b1yr <- raster::extract(h12[[18]], area1[5,])
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NBRI2)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_6=gg[-c(8:14), ]
b1yr_6$indice = c("NBRI2")

b3yr_6=gg[-c(1:7), ]
b3yr_6$indice = c("NBRI2")
#NDII ===================================================================
crt <- raster::extract(h04[[6]], area1[1,]); b3yr <- raster::extract(h04[[6]], area1[3,]); b1yr <- raster::extract(h04[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii1 = as.data.frame(rbind(a, b, c))
ndii1 <- ndii1[,c(1,3)]
colnames(ndii1) = c("ndii", "parcela")
ndii1 = ndii1 %>% 
  mutate(data = "2004")
ndii1_md = ndii1 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[6]], area1[1,]); b3yr <- raster::extract(h05[[6]], area1[3,]); b1yr <- raster::extract(h05[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii2 = as.data.frame(rbind(a, b, c))
ndii2 <- ndii2[,c(1,3)]
colnames(ndii2) = c("ndii", "parcela")
ndii2 = ndii2 %>% 
  mutate(data = "2005")
ndii2_md = ndii2 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[6]], area1[1,]); b3yr <- raster::extract(h06[[6]], area1[3,]); b1yr <- raster::extract(h06[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii3 = as.data.frame(rbind(a, b, c))
ndii3 <- ndii3[,c(1,3)]
colnames(ndii3) = c("ndii", "parcela")
ndii3 = ndii3 %>% 
  mutate(data = "2006")
ndii3_md = ndii3 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[6]], area1[1,]); b3yr <- raster::extract(h08[[6]], area1[3,]); b1yr <- raster::extract(h08[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii4 = as.data.frame(rbind(a, b, c))
ndii4 <- ndii4[,c(1,3)]
colnames(ndii4) = c("ndii", "parcela")
ndii4 = ndii4 %>% 
  mutate(data = "2008")
ndii4_md = ndii4 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[6]], area1[1,]); b3yr <- raster::extract(h10[[6]], area1[3,]); b1yr <- raster::extract(h10[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii5 = as.data.frame(rbind(a, b, c))
ndii5 <- ndii5[,c(1,3)]
colnames(ndii5) = c("ndii", "parcela")
ndii5 = ndii5 %>% 
  mutate(data = "2010")
ndii5_md = ndii5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[6]], area1[1,]); b3yr <- raster::extract(h11[[6]], area1[3,]); b1yr <- raster::extract(h11[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii6 = as.data.frame(rbind(a, b, c))
ndii6 <- ndii6[,c(1,3)]
colnames(ndii6) = c("ndii", "parcela")
ndii6 = ndii6 %>% 
  mutate(data = "2011")
ndii6_md = ndii6 %>%
  group_by(parcela) %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[6]], area1[1,]); b3yr <- raster::extract(h12[[6]], area1[3,]); b1yr <- raster::extract(h12[[6]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndii7 = as.data.frame(rbind(a, b, c))
ndii7 <- ndii7[,c(1,3)]
colnames(ndii7) = c("ndii", "parcela")
ndii7 = ndii7 %>% 
  mutate(data = "2012")
ndii7_md = ndii7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndii = median(ndii)) %>% 
  mutate(data = "2012")


ndii = as.data.frame(rbind(ndii1, ndii2, ndii3, ndii4, ndii5, ndii6, ndii7))
ndii_md = as.data.frame(rbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))



#Boxplot
ggplot(ndii, aes(data,ndii, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NDII")+
  theme_minimal()

#Medians
ggplot(ndii_md, aes(data,ndii, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDII")+
  theme_minimal()

diff = as.data.frame(cbind(ndii1_md, ndii2_md, ndii3_md, ndii4_md, ndii5_md, ndii6_md, ndii7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NDII)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_7=gg[-c(8:14), ]
b1yr_7$indice = c("NDII")

b3yr_7=gg[-c(1:7), ]
b3yr_7$indice = c("NDII")
#NDVI ===================================================================
crt <- raster::extract(h04[[7]], area1[1,]); b3yr <- raster::extract(h04[[7]], area1[3,]); b1yr <- raster::extract(h04[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi1 = as.data.frame(rbind(a, b, c))
ndvi1 <- ndvi1[,c(1,3)]
colnames(ndvi1) = c("ndvi", "parcela")
ndvi1 = ndvi1 %>% 
  mutate(data = "2004")
ndvi1_md = ndvi1 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[7]], area1[1,]); b3yr <- raster::extract(h05[[7]], area1[3,]); b1yr <- raster::extract(h05[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi2 = as.data.frame(rbind(a, b, c))
ndvi2 <- ndvi2[,c(1,3)]
colnames(ndvi2) = c("ndvi", "parcela")
ndvi2 = ndvi2 %>% 
  mutate(data = "2005")
ndvi2_md = ndvi2 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[7]], area1[1,]); b3yr <- raster::extract(h06[[7]], area1[3,]); b1yr <- raster::extract(h06[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi3 = as.data.frame(rbind(a, b, c))
ndvi3 <- ndvi3[,c(1,3)]
colnames(ndvi3) = c("ndvi", "parcela")
ndvi3 = ndvi3 %>% 
  mutate(data = "2006")
ndvi3_md = ndvi3 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[7]], area1[1,]); b3yr <- raster::extract(h08[[7]], area1[3,]); b1yr <- raster::extract(h08[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi4 = as.data.frame(rbind(a, b, c))
ndvi4 <- ndvi4[,c(1,3)]
colnames(ndvi4) = c("ndvi", "parcela")
ndvi4 = ndvi4 %>% 
  mutate(data = "2008")
ndvi4_md = ndvi4 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[7]], area1[1,]); b3yr <- raster::extract(h10[[7]], area1[3,]); b1yr <- raster::extract(h10[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi5 = as.data.frame(rbind(a, b, c))
ndvi5 <- ndvi5[,c(1,3)]
colnames(ndvi5) = c("ndvi", "parcela")
ndvi5 = ndvi5 %>% 
  mutate(data = "2010")
ndvi5_md = ndvi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[7]], area1[1,]); b3yr <- raster::extract(h11[[7]], area1[3,]); b1yr <- raster::extract(h11[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi6 = as.data.frame(rbind(a, b, c))
ndvi6 <- ndvi6[,c(1,3)]
colnames(ndvi6) = c("ndvi", "parcela")
ndvi6 = ndvi6 %>% 
  mutate(data = "2011")
ndvi6_md = ndvi6 %>%
  group_by(parcela) %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[7]], area1[1,]); b3yr <- raster::extract(h12[[7]], area1[3,]); b1yr <- raster::extract(h12[[7]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndvi7 = as.data.frame(rbind(a, b, c))
ndvi7 <- ndvi7[,c(1,3)]
colnames(ndvi7) = c("ndvi", "parcela")
ndvi7 = ndvi7 %>% 
  mutate(data = "2012")
ndvi7_md = ndvi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndvi = median(ndvi)) %>% 
  mutate(data = "2012")


ndvi = as.data.frame(rbind(ndvi1, ndvi2, ndvi3, ndvi4, ndvi5, ndvi6, ndvi7))
ndvi_md = as.data.frame(rbind(ndvi1_md, ndvi2_md, ndvi3_md, ndvi4_md, ndvi5_md, ndvi6_md, ndvi7_md))



#Boxplot
ggplot(ndvi, aes(data,ndvi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  theme_minimal()

#Medians
ggplot(ndvi_md, aes(data,ndvi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  theme_minimal()

diff = as.data.frame(cbind(ndvi1_md, ndvi2_md, ndvi3_md, ndvi4_md, ndvi5_md, ndvi6_md, ndvi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NDVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_8=gg[-c(8:14), ]
b1yr_8$indice = c("NDVI")

b3yr_8=gg[-c(1:7), ]
b3yr_8$indice = c("NDVI")
#NDWI ===================================================================
crt <- raster::extract(h04[[8]], area1[1,]); b3yr <- raster::extract(h04[[8]], area1[3,]); b1yr <- raster::extract(h04[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi1 = as.data.frame(rbind(a, b, c))
ndwi1 <- ndwi1[,c(1,3)]
colnames(ndwi1) = c("ndwi", "parcela")
ndwi1 = ndwi1 %>% 
  mutate(data = "2004")
ndwi1_md = ndwi1 %>%
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[8]], area1[1,]); b3yr <- raster::extract(h05[[8]], area1[3,]); b1yr <- raster::extract(h05[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi2 = as.data.frame(rbind(a, b, c))
ndwi2 <- ndwi2[,c(1,3)]
colnames(ndwi2) = c("ndwi", "parcela")
ndwi2 = ndwi2 %>% 
  mutate(data = "2005")
ndwi2_md = ndwi2 %>%
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[8]], area1[1,]); b3yr <- raster::extract(h06[[8]], area1[3,]); b1yr <- raster::extract(h06[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi3 = as.data.frame(rbind(a, b, c))
ndwi3 <- ndwi3[,c(1,3)]
colnames(ndwi3) = c("ndwi", "parcela")
ndwi3 = ndwi3 %>% 
  mutate(data = "2006")
ndwi3_md = ndwi3 %>%
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[8]], area1[1,]); b3yr <- raster::extract(h08[[8]], area1[3,]); b1yr <- raster::extract(h08[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi4 = as.data.frame(rbind(a, b, c))
ndwi4 <- ndwi4[,c(1,3)]
colnames(ndwi4) = c("ndwi", "parcela")
ndwi4 = ndwi4 %>% 
  mutate(data = "2008")
ndwi4_md = ndwi4 %>%
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[8]], area1[1,]); b3yr <- raster::extract(h10[[8]], area1[3,]); b1yr <- raster::extract(h10[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi5 = as.data.frame(rbind(a, b, c))
ndwi5 <- ndwi5[,c(1,3)]
colnames(ndwi5) = c("ndwi", "parcela")
ndwi5 = ndwi5 %>% 
  mutate(data = "2010")
ndwi5_md = ndwi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[8]], area1[1,]); b3yr <- raster::extract(h11[[8]], area1[3,]); b1yr <- raster::extract(h11[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi6 = as.data.frame(rbind(a, b, c))
ndwi6 <- ndwi6[,c(1,3)]
colnames(ndwi6) = c("ndwi", "parcela")
ndwi6 = ndwi6 %>% 
  mutate(data = "2011")
ndwi6_md = ndwi6 %>%
  group_by(parcela) %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[8]], area1[1,]); b3yr <- raster::extract(h12[[8]], area1[3,]); b1yr <- raster::extract(h12[[8]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
ndwi7 = as.data.frame(rbind(a, b, c))
ndwi7 <- ndwi7[,c(1,3)]
colnames(ndwi7) = c("ndwi", "parcela")
ndwi7 = ndwi7 %>% 
  mutate(data = "2012")
ndwi7_md = ndwi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(ndwi = median(ndwi)) %>% 
  mutate(data = "2012")


ndwi = as.data.frame(rbind(ndwi1, ndwi2, ndwi3, ndwi4, ndwi5, ndwi6, ndwi7))
ndwi_md = as.data.frame(rbind(ndwi1_md, ndwi2_md, ndwi3_md, ndwi4_md, ndwi5_md, ndwi6_md, ndwi7_md))


#Boxplot
ggplot(ndwi, aes(data,ndwi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NDWI")+
  theme_minimal()

#Medians
ggplot(ndwi_md, aes(data,ndwi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDWI")+
  theme_minimal()

diff = as.data.frame(cbind(ndwi1_md, ndwi2_md, ndwi3_md, ndwi4_md, ndwi5_md, ndwi6_md, ndwi7_md))
diff = diff[,c(1,2,5,8,11,14,17,20)]
colnames(diff) = c("parcela","2004","2005","2006","2008","2010","2011","2012")
diff <- as.data.frame(t(diff))
colnames(diff) = c("b1yr","b3yr","controle")
diff <- diff[-c(1), ]

diff$b3yr = as.numeric(as.character(diff$b3yr))
diff$b1yr = as.numeric(as.character(diff$b1yr))
diff$controle = as.numeric(as.character(diff$controle))


#NDWI needs normalize to calculate
diff$b3yr = (((diff$b3yr-diff$controle)*100)/diff$controle)/100
diff$b1yr = (((diff$b1yr-diff$controle)*100)/diff$controle)/100
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NDWI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_9=gg[-c(8:14), ]
b1yr_9$indice = c("NDWI")

b3yr_9=gg[-c(1:7), ]
b3yr_9$indice = c("NDWI")
#NIRV ===================================================================
crt <- raster::extract(h04[[9]], area1[1,]); b3yr <- raster::extract(h04[[9]], area1[3,]); b1yr <- raster::extract(h04[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv1 = as.data.frame(rbind(a, b, c))
nirv1 <- nirv1[,c(1,3)]
colnames(nirv1) = c("nirv", "parcela")
nirv1 = nirv1 %>% 
  mutate(data = "2004")
nirv1_md = nirv1 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[9]], area1[1,]); b3yr <- raster::extract(h05[[9]], area1[3,]); b1yr <- raster::extract(h05[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv2 = as.data.frame(rbind(a, b, c))
nirv2 <- nirv2[,c(1,3)]
colnames(nirv2) = c("nirv", "parcela")
nirv2 = nirv2 %>% 
  mutate(data = "2005")
nirv2_md = nirv2 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[9]], area1[1,]); b3yr <- raster::extract(h06[[9]], area1[3,]); b1yr <- raster::extract(h06[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv3 = as.data.frame(rbind(a, b, c))
nirv3 <- nirv3[,c(1,3)]
colnames(nirv3) = c("nirv", "parcela")
nirv3 = nirv3 %>% 
  mutate(data = "2006")
nirv3_md = nirv3 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[9]], area1[1,]); b3yr <- raster::extract(h08[[9]], area1[3,]); b1yr <- raster::extract(h08[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv4 = as.data.frame(rbind(a, b, c))
nirv4 <- nirv4[,c(1,3)]
colnames(nirv4) = c("nirv", "parcela")
nirv4 = nirv4 %>% 
  mutate(data = "2008")
nirv4_md = nirv4 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[9]], area1[1,]); b3yr <- raster::extract(h10[[9]], area1[3,]); b1yr <- raster::extract(h10[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv5 = as.data.frame(rbind(a, b, c))
nirv5 <- nirv5[,c(1,3)]
colnames(nirv5) = c("nirv", "parcela")
nirv5 = nirv5 %>% 
  mutate(data = "2010")
nirv5_md = nirv5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[9]], area1[1,]); b3yr <- raster::extract(h11[[9]], area1[3,]); b1yr <- raster::extract(h11[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv6 = as.data.frame(rbind(a, b, c))
nirv6 <- nirv6[,c(1,3)]
colnames(nirv6) = c("nirv", "parcela")
nirv6 = nirv6 %>% 
  mutate(data = "2011")
nirv6_md = nirv6 %>%
  group_by(parcela) %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[9]], area1[1,]); b3yr <- raster::extract(h12[[9]], area1[3,]); b1yr <- raster::extract(h12[[9]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
nirv7 = as.data.frame(rbind(a, b, c))
nirv7 <- nirv7[,c(1,3)]
colnames(nirv7) = c("nirv", "parcela")
nirv7 = nirv7 %>% 
  mutate(data = "2012")
nirv7_md = nirv7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(nirv = median(nirv)) %>% 
  mutate(data = "2012")


nirv = as.data.frame(rbind(nirv1, nirv2, nirv3, nirv4, nirv5, nirv6, nirv7))
nirv_md = as.data.frame(rbind(nirv1_md, nirv2_md, nirv3_md, nirv4_md, nirv5_md, nirv6_md, nirv7_md))



#Boxplot
ggplot(nirv, aes(data,nirv, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="NIRV")+
  theme_minimal()

#Medians
ggplot(nirv_md, aes(data,nirv, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NIRV")+
  theme_minimal()

diff = as.data.frame(cbind(nirv1_md, nirv2_md, nirv3_md, nirv4_md, nirv5_md, nirv6_md, nirv7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no NIRV)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_10=gg[-c(8:14), ]
b1yr_10$indice = c("NIRV")

b3yr_10=gg[-c(1:7), ]
b3yr_10$indice = c("NIRV")
#PRI ===================================================================
crt <- raster::extract(h04[[10]], area1[1,]); b3yr <- raster::extract(h04[[10]], area1[3,]); b1yr <- raster::extract(h04[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri1 = as.data.frame(rbind(a, b, c))
pri1 <- pri1[,c(1,3)]
colnames(pri1) = c("pri", "parcela")
pri1 = pri1 %>% 
  mutate(data = "2004")
pri1_md = pri1 %>%
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[10]], area1[1,]); b3yr <- raster::extract(h05[[10]], area1[3,]); b1yr <- raster::extract(h05[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri2 = as.data.frame(rbind(a, b, c))
pri2 <- pri2[,c(1,3)]
colnames(pri2) = c("pri", "parcela")
pri2 = pri2 %>% 
  mutate(data = "2005")
pri2_md = pri2 %>%
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[10]], area1[1,]); b3yr <- raster::extract(h06[[10]], area1[3,]); b1yr <- raster::extract(h06[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri3 = as.data.frame(rbind(a, b, c))
pri3 <- pri3[,c(1,3)]
colnames(pri3) = c("pri", "parcela")
pri3 = pri3 %>% 
  mutate(data = "2006")
pri3_md = pri3 %>%
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[10]], area1[1,]); b3yr <- raster::extract(h08[[10]], area1[3,]); b1yr <- raster::extract(h08[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri4 = as.data.frame(rbind(a, b, c))
pri4 <- pri4[,c(1,3)]
colnames(pri4) = c("pri", "parcela")
pri4 = pri4 %>% 
  mutate(data = "2008")
pri4_md = pri4 %>%
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[10]], area1[1,]); b3yr <- raster::extract(h10[[10]], area1[3,]); b1yr <- raster::extract(h10[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri5 = as.data.frame(rbind(a, b, c))
pri5 <- pri5[,c(1,3)]
colnames(pri5) = c("pri", "parcela")
pri5 = pri5 %>% 
  mutate(data = "2010")
pri5_md = pri5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[10]], area1[1,]); b3yr <- raster::extract(h11[[10]], area1[3,]); b1yr <- raster::extract(h11[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri6 = as.data.frame(rbind(a, b, c))
pri6 <- pri6[,c(1,3)]
colnames(pri6) = c("pri", "parcela")
pri6 = pri6 %>% 
  mutate(data = "2011")
pri6_md = pri6 %>%
  group_by(parcela) %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[10]], area1[1,]); b3yr <- raster::extract(h12[[10]], area1[3,]); b1yr <- raster::extract(h12[[10]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
pri7 = as.data.frame(rbind(a, b, c))
pri7 <- pri7[,c(1,3)]
colnames(pri7) = c("pri", "parcela")
pri7 = pri7 %>% 
  mutate(data = "2012")
pri7_md = pri7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(pri = median(pri)) %>% 
  mutate(data = "2012")


pri = as.data.frame(rbind(pri1, pri2, pri3, pri4, pri5, pri6, pri7))
pri_md = as.data.frame(rbind(pri1_md, pri2_md, pri3_md, pri4_md, pri5_md, pri6_md, pri7_md))



#Boxplot
ggplot(pri, aes(data,pri, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="PRI")+
  theme_minimal()

#Medians
ggplot(pri_md, aes(data,pri, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="PRI")+
  theme_minimal()

diff = as.data.frame(cbind(pri1_md, pri2_md, pri3_md, pri4_md, pri5_md, pri6_md, pri7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no PRI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_11=gg[-c(8:14), ]
b1yr_11$indice = c("PRI")

b3yr_11=gg[-c(1:7), ]
b3yr_11$indice = c("PRI")
#PSRI ===================================================================
crt <- raster::extract(h04[[11]], area1[1,]); b3yr <- raster::extract(h04[[11]], area1[3,]); b1yr <- raster::extract(h04[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri1 = as.data.frame(rbind(a, b, c))
psri1 <- psri1[,c(1,3)]
colnames(psri1) = c("psri", "parcela")
psri1 = psri1 %>% 
  mutate(data = "2004")
psri1_md = psri1 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[11]], area1[1,]); b3yr <- raster::extract(h05[[11]], area1[3,]); b1yr <- raster::extract(h05[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri2 = as.data.frame(rbind(a, b, c))
psri2 <- psri2[,c(1,3)]
colnames(psri2) = c("psri", "parcela")
psri2 = psri2 %>% 
  mutate(data = "2005")
psri2_md = psri2 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[11]], area1[1,]); b3yr <- raster::extract(h06[[11]], area1[3,]); b1yr <- raster::extract(h06[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri3 = as.data.frame(rbind(a, b, c))
psri3 <- psri3[,c(1,3)]
colnames(psri3) = c("psri", "parcela")
psri3 = psri3 %>% 
  mutate(data = "2006")
psri3_md = psri3 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[11]], area1[1,]); b3yr <- raster::extract(h08[[11]], area1[3,]); b1yr <- raster::extract(h08[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri4 = as.data.frame(rbind(a, b, c))
psri4 <- psri4[,c(1,3)]
colnames(psri4) = c("psri", "parcela")
psri4 = psri4 %>% 
  mutate(data = "2008")
psri4_md = psri4 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[11]], area1[1,]); b3yr <- raster::extract(h10[[11]], area1[3,]); b1yr <- raster::extract(h10[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri5 = as.data.frame(rbind(a, b, c))
psri5 <- psri5[,c(1,3)]
colnames(psri5) = c("psri", "parcela")
psri5 = psri5 %>% 
  mutate(data = "2010")
psri5_md = psri5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[11]], area1[1,]); b3yr <- raster::extract(h11[[11]], area1[3,]); b1yr <- raster::extract(h11[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri6 = as.data.frame(rbind(a, b, c))
psri6 <- psri6[,c(1,3)]
colnames(psri6) = c("psri", "parcela")
psri6 = psri6 %>% 
  mutate(data = "2011")
psri6_md = psri6 %>%
  group_by(parcela) %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[11]], area1[1,]); b3yr <- raster::extract(h12[[11]], area1[3,]); b1yr <- raster::extract(h12[[11]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
psri7 = as.data.frame(rbind(a, b, c))
psri7 <- psri7[,c(1,3)]
colnames(psri7) = c("psri", "parcela")
psri7 = psri7 %>% 
  mutate(data = "2012")
psri7_md = psri7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(psri = median(psri)) %>% 
  mutate(data = "2012")


psri = as.data.frame(rbind(psri1, psri2, psri3, psri4, psri5, psri6, psri7))
psri_md = as.data.frame(rbind(psri1_md, psri2_md, psri3_md, psri4_md, psri5_md, psri6_md, psri7_md))



#Boxplot
ggplot(psri, aes(data,psri, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="PSRI")+
  theme_minimal()

#Medians
ggplot(psri_md, aes(data,psri, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="PSRI")+
  theme_minimal()

diff = as.data.frame(cbind(psri1_md, psri2_md, psri3_md, psri4_md, psri5_md, psri6_md, psri7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no PSRI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_12=gg[-c(8:14), ]
b1yr_12$indice = c("PSRI")

b3yr_12=gg[-c(1:7), ]
b3yr_12$indice = c("PSRI")
#PSSR ===================================================================
crt <- raster::extract(h04[[12]], area1[1,]); b3yr <- raster::extract(h04[[12]], area1[3,]); b1yr <- raster::extract(h04[[12]], area1[5,])
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

crt <- raster::extract(h05[[12]], area1[1,]); b3yr <- raster::extract(h05[[12]], area1[3,]); b1yr <- raster::extract(h05[[12]], area1[5,])
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

crt <- raster::extract(h06[[12]], area1[1,]); b3yr <- raster::extract(h06[[12]], area1[3,]); b1yr <- raster::extract(h06[[12]], area1[5,])
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

crt <- raster::extract(h08[[12]], area1[1,]); b3yr <- raster::extract(h08[[12]], area1[3,]); b1yr <- raster::extract(h08[[12]], area1[5,])
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

crt <- raster::extract(h10[[12]], area1[1,]); b3yr <- raster::extract(h10[[12]], area1[3,]); b1yr <- raster::extract(h10[[12]], area1[5,])
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

crt <- raster::extract(h11[[12]], area1[1,]); b3yr <- raster::extract(h11[[12]], area1[3,]); b1yr <- raster::extract(h11[[12]], area1[5,])
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

crt <- raster::extract(h12[[12]], area1[1,]); b3yr <- raster::extract(h12[[12]], area1[3,]); b1yr <- raster::extract(h12[[12]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no PSSR)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_13=gg[-c(8:14), ]
b1yr_13$indice = c("PSSR")

b3yr_13=gg[-c(1:7), ]
b3yr_13$indice = c("PSSR")
#RENDVI ===================================================================
crt <- raster::extract(h04[[13]], area1[1,]); b3yr <- raster::extract(h04[[13]], area1[3,]); b1yr <- raster::extract(h04[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi1 = as.data.frame(rbind(a, b, c))
rendvi1 <- rendvi1[,c(1,3)]
colnames(rendvi1) = c("rendvi", "parcela")
rendvi1 = rendvi1 %>% 
  mutate(data = "2004")
rendvi1_md = rendvi1 %>%
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2004")

crt <- raster::extract(h05[[13]], area1[1,]); b3yr <- raster::extract(h05[[13]], area1[3,]); b1yr <- raster::extract(h05[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi2 = as.data.frame(rbind(a, b, c))
rendvi2 <- rendvi2[,c(1,3)]
colnames(rendvi2) = c("rendvi", "parcela")
rendvi2 = rendvi2 %>% 
  mutate(data = "2005")
rendvi2_md = rendvi2 %>%
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2005")

crt <- raster::extract(h06[[13]], area1[1,]); b3yr <- raster::extract(h06[[13]], area1[3,]); b1yr <- raster::extract(h06[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi3 = as.data.frame(rbind(a, b, c))
rendvi3 <- rendvi3[,c(1,3)]
colnames(rendvi3) = c("rendvi", "parcela")
rendvi3 = rendvi3 %>% 
  mutate(data = "2006")
rendvi3_md = rendvi3 %>%
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2006")

crt <- raster::extract(h08[[13]], area1[1,]); b3yr <- raster::extract(h08[[13]], area1[3,]); b1yr <- raster::extract(h08[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi4 = as.data.frame(rbind(a, b, c))
rendvi4 <- rendvi4[,c(1,3)]
colnames(rendvi4) = c("rendvi", "parcela")
rendvi4 = rendvi4 %>% 
  mutate(data = "2008")
rendvi4_md = rendvi4 %>%
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2008")

crt <- raster::extract(h10[[13]], area1[1,]); b3yr <- raster::extract(h10[[13]], area1[3,]); b1yr <- raster::extract(h10[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi5 = as.data.frame(rbind(a, b, c))
rendvi5 <- rendvi5[,c(1,3)]
colnames(rendvi5) = c("rendvi", "parcela")
rendvi5 = rendvi5 %>% 
  mutate(data = "2010")
rendvi5_md = rendvi5 %>%
  na.omit() %>% 
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2010")

crt <- raster::extract(h11[[13]], area1[1,]); b3yr <- raster::extract(h11[[13]], area1[3,]); b1yr <- raster::extract(h11[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi6 = as.data.frame(rbind(a, b, c))
rendvi6 <- rendvi6[,c(1,3)]
colnames(rendvi6) = c("rendvi", "parcela")
rendvi6 = rendvi6 %>% 
  mutate(data = "2011")
rendvi6_md = rendvi6 %>%
  group_by(parcela) %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2011")

crt <- raster::extract(h12[[13]], area1[1,]); b3yr <- raster::extract(h12[[13]], area1[3,]); b1yr <- raster::extract(h12[[13]], area1[5,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
rendvi7 = as.data.frame(rbind(a, b, c))
rendvi7 <- rendvi7[,c(1,3)]
colnames(rendvi7) = c("rendvi", "parcela")
rendvi7 = rendvi7 %>% 
  mutate(data = "2012")
rendvi7_md = rendvi7 %>%
  group_by(parcela) %>% 
  na.omit() %>% 
  summarise(rendvi = median(rendvi)) %>% 
  mutate(data = "2012")


rendvi = as.data.frame(rbind(rendvi1, rendvi2, rendvi3, rendvi4, rendvi5, rendvi6, rendvi7))
rendvi_md = as.data.frame(rbind(rendvi1_md, rendvi2_md, rendvi3_md, rendvi4_md, rendvi5_md, rendvi6_md, rendvi7_md))



#Boxplot
ggplot(rendvi, aes(data,rendvi, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="RENDVI")+
  theme_minimal()

#Medians
ggplot(rendvi_md, aes(data,rendvi, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="RENDVI")+
  theme_minimal()

diff = as.data.frame(cbind(rendvi1_md, rendvi2_md, rendvi3_md, rendvi4_md, rendvi5_md, rendvi6_md, rendvi7_md))
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
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no RENDVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_14=gg[-c(8:14), ]
b1yr_14$indice = c("RENDVI")

b3yr_14=gg[-c(1:7), ]
b3yr_14$indice = c("RENDVI")
#SIPI ===================================================================
crt <- raster::extract(h04[[14]], area1[1,]); b3yr <- raster::extract(h04[[14]], area1[3,]); b1yr <- raster::extract(h04[[14]], area1[5,])
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

crt <- raster::extract(h05[[14]], area1[1,]); b3yr <- raster::extract(h05[[14]], area1[3,]); b1yr <- raster::extract(h05[[14]], area1[5,])
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

crt <- raster::extract(h06[[14]], area1[1,]); b3yr <- raster::extract(h06[[14]], area1[3,]); b1yr <- raster::extract(h06[[14]], area1[5,])
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

crt <- raster::extract(h08[[14]], area1[1,]); b3yr <- raster::extract(h08[[14]], area1[3,]); b1yr <- raster::extract(h08[[14]], area1[5,])
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

crt <- raster::extract(h10[[14]], area1[1,]); b3yr <- raster::extract(h10[[14]], area1[3,]); b1yr <- raster::extract(h10[[14]], area1[5,])
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

crt <- raster::extract(h11[[14]], area1[1,]); b3yr <- raster::extract(h11[[14]], area1[3,]); b1yr <- raster::extract(h11[[14]], area1[5,])
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

crt <- raster::extract(h12[[14]], area1[1,]); b3yr <- raster::extract(h12[[14]], area1[3,]); b1yr <- raster::extract(h12[[14]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no SIPI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_15=gg[-c(8:14), ]
b1yr_15$indice = c("SIPI")

b3yr_15=gg[-c(1:7), ]
b3yr_15$indice = c("SIPI")
#VARI ===================================================================
crt <- raster::extract(h04[[15]], area1[1,]); b3yr <- raster::extract(h04[[15]], area1[3,]); b1yr <- raster::extract(h04[[15]], area1[5,])
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

crt <- raster::extract(h05[[15]], area1[1,]); b3yr <- raster::extract(h05[[15]], area1[3,]); b1yr <- raster::extract(h05[[15]], area1[5,])
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

crt <- raster::extract(h06[[15]], area1[1,]); b3yr <- raster::extract(h06[[15]], area1[3,]); b1yr <- raster::extract(h06[[15]], area1[5,])
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

crt <- raster::extract(h08[[15]], area1[1,]); b3yr <- raster::extract(h08[[15]], area1[3,]); b1yr <- raster::extract(h08[[15]], area1[5,])
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

crt <- raster::extract(h10[[15]], area1[1,]); b3yr <- raster::extract(h10[[15]], area1[3,]); b1yr <- raster::extract(h10[[15]], area1[5,])
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

crt <- raster::extract(h11[[15]], area1[1,]); b3yr <- raster::extract(h11[[15]], area1[3,]); b1yr <- raster::extract(h11[[15]], area1[5,])
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

crt <- raster::extract(h12[[15]], area1[1,]); b3yr <- raster::extract(h12[[15]], area1[3,]); b1yr <- raster::extract(h12[[15]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no VARI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_16=gg[-c(8:14), ]
b1yr_16$indice = c("VARI")

b3yr_16=gg[-c(1:7), ]
b3yr_16$indice = c("VARI")
#VIG ===================================================================
crt <- raster::extract(h04[[16]], area1[1,]); b3yr <- raster::extract(h04[[16]], area1[3,]); b1yr <- raster::extract(h04[[16]], area1[5,])
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

crt <- raster::extract(h05[[16]], area1[1,]); b3yr <- raster::extract(h05[[16]], area1[3,]); b1yr <- raster::extract(h05[[16]], area1[5,])
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

crt <- raster::extract(h06[[16]], area1[1,]); b3yr <- raster::extract(h06[[16]], area1[3,]); b1yr <- raster::extract(h06[[16]], area1[5,])
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

crt <- raster::extract(h08[[16]], area1[1,]); b3yr <- raster::extract(h08[[16]], area1[3,]); b1yr <- raster::extract(h08[[16]], area1[5,])
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

crt <- raster::extract(h10[[16]], area1[1,]); b3yr <- raster::extract(h10[[16]], area1[3,]); b1yr <- raster::extract(h10[[16]], area1[5,])
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

crt <- raster::extract(h11[[16]], area1[1,]); b3yr <- raster::extract(h11[[16]], area1[3,]); b1yr <- raster::extract(h11[[16]], area1[5,])
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

crt <- raster::extract(h12[[16]], area1[1,]); b3yr <- raster::extract(h12[[16]], area1[3,]); b1yr <- raster::extract(h12[[16]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no VIG)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_17=gg[-c(8:14), ]
b1yr_17$indice = c("VIG")

b3yr_17=gg[-c(1:7), ]
b3yr_17$indice = c("VIG")
#WBI ===================================================================
crt <- raster::extract(h04[[17]], area1[1,]); b3yr <- raster::extract(h04[[17]], area1[3,]); b1yr <- raster::extract(h04[[17]], area1[5,])
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

crt <- raster::extract(h05[[17]], area1[1,]); b3yr <- raster::extract(h05[[17]], area1[3,]); b1yr <- raster::extract(h05[[17]], area1[5,])
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

crt <- raster::extract(h06[[17]], area1[1,]); b3yr <- raster::extract(h06[[17]], area1[3,]); b1yr <- raster::extract(h06[[17]], area1[5,])
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

crt <- raster::extract(h08[[17]], area1[1,]); b3yr <- raster::extract(h08[[17]], area1[3,]); b1yr <- raster::extract(h08[[17]], area1[5,])
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

crt <- raster::extract(h10[[17]], area1[1,]); b3yr <- raster::extract(h10[[17]], area1[3,]); b1yr <- raster::extract(h10[[17]], area1[5,])
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

crt <- raster::extract(h11[[17]], area1[1,]); b3yr <- raster::extract(h11[[17]], area1[3,]); b1yr <- raster::extract(h11[[17]], area1[5,])
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

crt <- raster::extract(h12[[17]], area1[1,]); b3yr <- raster::extract(h12[[17]], area1[3,]); b1yr <- raster::extract(h12[[17]], area1[5,])
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
  geom_boxplot(outlier.alpha = 0.1)+
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

diff$b3yr = ((diff$b3yr-diff$controle)*100)/diff$controle
diff$b1yr = ((diff$b1yr-diff$controle)*100)/diff$controle
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% Relative difference no WBI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

b1yr_18=gg[-c(8:14), ]
b1yr_18$indice = c("WBI")

b3yr_18=gg[-c(1:7), ]
b3yr_18$indice = c("WBI")

#Union to save medians data frame to analysis ====
nbri_md = ndii_md #It is the same index
colnames(nbri_md) = c('parcela', 'nbri', 'data')

hy = cbind(ari_md, evi_md, lwvi2_md, msi_md, nbri_md, nbri2_md, ndii_md, ndvi_md, ndwi_md,
           nirv_md, pri_md, psri_md, pssr_md, rendvi_md, sipi_md, vari_md, vig_md, wbi_md)

hy = hy[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,54)]
hy$dist = as.character("borda")

#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(hy, file = "Hyperion_indexs_median by plot(borda).csv", sep = ",")

#Union all differences by index ===================================================
b3yr = rbind(b3yr_1, b3yr_2, b3yr_3, b3yr_4, b3yr_5, b3yr_6, b3yr_7, b3yr_8, b3yr_9, b3yr_10, b3yr_11, b3yr_12, b3yr_13, b3yr_14, b3yr_15, b3yr_16, b3yr_17, b3yr_18)

b1yr = rbind(b1yr_1, b1yr_2, b1yr_3, b1yr_4, b1yr_5, b1yr_6, b1yr_7, b1yr_8, b1yr_9, b1yr_10, b1yr_11, b1yr_12, b1yr_13, b1yr_14, b1yr_15, b1yr_16, b1yr_17, b1yr_18)


ggplot(b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplot(b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  #scale_color_viridis(discrete = TRUE, option = "plasma") +
  #geom_point()+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

#Union by the groups ===============================================================
#Struture
struc_b3yr = rbind(b3yr_2, b3yr_8, b3yr_10, b3yr_16, b3yr_17)
struc_b1yr = rbind(b1yr_2, b1yr_8, b1yr_10, b1yr_16, b1yr_17)

ggplot(struc_b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Structural Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplot(struc_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Structural Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()


#Biochemistry
bio_b3yr = rbind(b3yr_1, b3yr_3, b3yr_4, b3yr_7, b3yr_9, b3yr_12, b3yr_13, b3yr_15, b3yr_18)
bio_b1yr = rbind(b1yr_1, b1yr_3, b1yr_4, b1yr_7, b1yr_9, b1yr_12, b1yr_13, b1yr_15, b1yr_18)

ggplot(bio_b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplot(bio_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

#Physiology
phy_b3yr = rbind(b3yr_11, b3yr_14)
phy_b1yr = rbind(b1yr_11, b1yr_14)

ggplot(phy_b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Physiology Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplot(phy_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Physiology Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

#Fire indices
fire_b3yr = rbind(b3yr_5, b3yr_6)
fire_b1yr = rbind(b1yr_5, b1yr_6)

ggplot(fire_b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Fire Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplot(fire_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B1yr - Controle (% Relative difference)",
       title = "Fire Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()






