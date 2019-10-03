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

fire <- stack(list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados rasterizados/Fogo", pattern = ".tif$", full.names=TRUE,recursive=TRUE))
fire = stack(fire[[1]], fire[[2]], fire[[3]], fire[[5]], fire[[7]])#Select only the data that match with hyperion data


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

#Save litter means data frame to analysis =======
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(litt_m, file = "Litter_mean by plot.csv", sep = ",")

#Same plots to view results
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


#Fogo ===================================================================
b1yr <- raster::extract(fire[[1]], area1[3,]); crt <- raster::extract(fire[[1]], area1[1,]); b3yr <- raster::extract(fire[[1]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
fire1 = as.data.frame(rbind(a, b, c))
fire1 <- fire1[,c(1,3)]
colnames(fire1) = c("fogo", "parcela")
fire1 = fire1 %>% 
  na.omit() %>% 
  mutate(data = "2004")
fire1_m = fire1 %>%
  group_by(parcela) %>% 
  summarise(fogo = mean(fogo)) %>% 
  mutate(data = "2004")

b1yr <- raster::extract(fire[[2]], area1[3,]); crt <- raster::extract(fire[[2]], area1[1,]); b3yr <- raster::extract(fire[[2]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
fire2 = as.data.frame(rbind(a, b, c))
fire2 <- fire2[,c(1,3)]
colnames(fire2) = c("fogo", "parcela")
fire2 = fire2 %>% 
  na.omit() %>% 
  mutate(data = "2005")
fire2_m = fire2 %>%
  group_by(parcela) %>% 
  summarise(fogo = mean(fogo)) %>% 
  mutate(data = "2005")

b1yr <- raster::extract(fire[[3]], area1[3,]); crt <- raster::extract(fire[[3]], area1[1,]); b3yr <- raster::extract(fire[[3]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
fire3 = as.data.frame(rbind(a, b, c))
fire3 <- fire3[,c(1,3)]
colnames(fire3) = c("fogo", "parcela")
fire3 = fire3 %>% 
  na.omit() %>% 
  mutate(data = "2006")
fire3_m = fire3 %>%
  group_by(parcela) %>% 
  summarise(fogo = mean(fogo)) %>% 
  mutate(data = "2006")

b1yr <- raster::extract(fire[[4]], area1[3,]); crt <- raster::extract(fire[[4]], area1[1,]); b3yr <- raster::extract(fire[[4]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
fire4 = as.data.frame(rbind(a, b, c))
fire4 <- fire4[,c(1,3)]
colnames(fire4) = c("fogo", "parcela")
fire4 = fire4 %>% 
  na.omit() %>% 
  mutate(data = "2008")
fire4_m = fire4 %>%
  group_by(parcela) %>% 
  summarise(fogo = mean(fogo)) %>% 
  mutate(data = "2008")

b1yr <- raster::extract(fire[[5]], area1[3,]); crt <- raster::extract(fire[[5]], area1[1,]); b3yr <- raster::extract(fire[[5]], area1[2,])
a <- melt(crt); c <- melt(b1yr); b <- melt(b3yr)
a = a %>% 
  mutate(parcela = "controle")
b = b %>% 
  mutate(parcela = "b3yr")
c = c %>% 
  mutate(parcela = "b1yr")
fire5 = as.data.frame(rbind(a, b, c))
fire5 <- fire5[,c(1,3)]
colnames(fire5) = c("fogo", "parcela")
fire5 = fire5 %>% 
  na.omit() %>% 
  mutate(data = "2010")
fire5_m = fire5 %>%
  group_by(parcela) %>% 
  summarise(fogo = mean(fogo)) %>% 
  mutate(data = "2010")


fire = as.data.frame(rbind(fire1, fire2, fire3, fire4, fire5))
fire_m = as.data.frame(rbind(fire1_m, fire2_m, fire3_m, fire4_m, fire5_m))

#Save fire means data frame to analysis ====
#setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")
#write.csv(fire_m, file = "fireer_mean by plot.csv", sep = ",")

#Same plots to view results
#Boxplot
ggplot(fire, aes(data,fogo, col=parcela))+ 
  geom_boxplot(outlier.alpha = 0.1)+
  labs(fill= "Plot",x="Ano",y="fire")+
  theme_minimal()

#Medians
ggplot(fire_m, aes(data,fogo, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="fogo")+
  theme_minimal()

#Extract difference
diff = as.data.frame(cbind(fire1_m, fire2_m, fire3_m, fire4_m, fire5_m))
diff = diff[,c(1,2,5,8,11,14)]
colnames(diff) = c("parcela","2004","2005","2006","2008","2010")
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
colnames(gg) = c("parcela","fire")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2004, 2005, 2006, 2008, 2010)


ggplot(gg, aes(data,fire, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fire - Control (% Relative difference on fireer)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()