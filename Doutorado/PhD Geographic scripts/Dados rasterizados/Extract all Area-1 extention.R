#Extract information from rasterized Area-1 data
#For all plot extention
# Eduardo Q Marques  02/10/2019

#Obs: It's to the same years that Hyperion data!

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
b1yr <- raster::extract(litt[[1]], area1[3,]); crt <- raster::extract(litt[[1]], area1[1,]); b3yr <- raster::extract(litt[[1]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt1 = as.data.frame(rbind(a, b, c))
litt1 <- litt1[,c(1,3)]
colnames(litt1) = c("liteira", "parcela")
litt1 = litt1 %>% 
  na.omit() %>% 
  mutate(data = "2004")
litt1_m = litt1 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2004")

b1yr <- raster::extract(litt[[2]], area1[3,]); crt <- raster::extract(litt[[2]], area1[1,]); b3yr <- raster::extract(litt[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt2 = as.data.frame(rbind(a, b, c))
litt2 <- litt2[,c(1,3)]
colnames(litt2) = c("liteira", "parcela")
litt2 = litt2 %>% 
  na.omit() %>% 
  mutate(data = "2005")
litt2_m = litt2 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2005")

b1yr <- raster::extract(litt[[3]], area1[3,]); crt <- raster::extract(litt[[3]], area1[1,]); b3yr <- raster::extract(litt[[3]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt3 = as.data.frame(rbind(a, b, c))
litt3 <- litt3[,c(1,3)]
colnames(litt3) = c("liteira", "parcela")
litt3 = litt3 %>% 
  na.omit() %>% 
  mutate(data = "2006")
litt3_m = litt3 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2006")

b1yr <- raster::extract(litt[[4]], area1[3,]); crt <- raster::extract(litt[[4]], area1[1,]); b3yr <- raster::extract(litt[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt4 = as.data.frame(rbind(a, b, c))
litt4 <- litt4[,c(1,3)]
colnames(litt4) = c("liteira", "parcela")
litt4 = litt4 %>% 
  na.omit() %>% 
  mutate(data = "2008")
litt4_m = litt4 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2008")

b1yr <- raster::extract(litt[[5]], area1[3,]); crt <- raster::extract(litt[[5]], area1[1,]); b3yr <- raster::extract(litt[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt5 = as.data.frame(rbind(a, b, c))
litt5 <- litt5[,c(1,3)]
colnames(litt5) = c("liteira", "parcela")
litt5 = litt5 %>% 
  na.omit() %>% 
  mutate(data = "2010")
litt5_m = litt5 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2010")

b1yr <- raster::extract(litt[[6]], area1[3,]); crt <- raster::extract(litt[[6]], area1[1,]); b3yr <- raster::extract(litt[[6]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt6 = as.data.frame(rbind(a, b, c))
litt6 <- litt6[,c(1,3)]
colnames(litt6) = c("liteira", "parcela")
litt6 = litt6 %>% 
  na.omit() %>% 
  mutate(data = "2011")
litt6_m = litt6 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2011")

b1yr <- raster::extract(litt[[7]], area1[3,]); crt <- raster::extract(litt[[7]], area1[1,]); b3yr <- raster::extract(litt[[7]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
litt7 = as.data.frame(rbind(a, b, c))
litt7 <- litt7[,c(1,3)]
colnames(litt7) = c("liteira", "parcela")
litt7 = litt7 %>% 
  na.omit() %>% 
  mutate(data = "2012")
litt7_m = litt7 %>%
  group_by(parcela) %>% 
  summarise(liteira = mean(liteira)) %>% 
  mutate(data = "2012")


litt = as.data.frame(rbind(litt1, litt2, litt3, litt4, litt5, litt6, litt7))
litt_m = as.data.frame(rbind(litt1_m, litt2_m, litt3_m, litt4_m, litt5_m, litt6_m, litt7_m))

#Boxplot
ggplot(litt, aes(data,liteira, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="litt")+
  theme_minimal()

#Medians
ggplot(litt_m, aes(data,liteira, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="liteira")+
  theme_minimal()

#Extract difference
diff = as.data.frame(cbind(litt1_m, litt2_m, litt3_m, litt4_m, litt5_m, litt6_m, litt7_m))
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
colnames(gg) = c("parcela","litter")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,litter, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fire - Control (% Relative difference on litter)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()
