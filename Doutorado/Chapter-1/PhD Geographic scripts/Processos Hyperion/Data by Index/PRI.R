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
crt <- raster::extract(h04[[10]], area1[3,]); b3yr <- raster::extract(h04[[10]], area1[1,]); b1yr <- raster::extract(h04[[10]], area1[2,])
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

crt <- raster::extract(h05[[10]], area1[3,]); b3yr <- raster::extract(h05[[10]], area1[1,]); b1yr <- raster::extract(h05[[10]], area1[2,])
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

crt <- raster::extract(h06[[10]], area1[3,]); b3yr <- raster::extract(h06[[10]], area1[1,]); b1yr <- raster::extract(h06[[10]], area1[2,])
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

crt <- raster::extract(h08[[10]], area1[3,]); b3yr <- raster::extract(h08[[10]], area1[1,]); b1yr <- raster::extract(h08[[10]], area1[2,])
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

crt <- raster::extract(h10[[10]], area1[3,]); b3yr <- raster::extract(h10[[10]], area1[1,]); b1yr <- raster::extract(h10[[10]], area1[2,])
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

crt <- raster::extract(h11[[10]], area1[3,]); b3yr <- raster::extract(h11[[10]], area1[1,]); b1yr <- raster::extract(h11[[10]], area1[2,])
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

crt <- raster::extract(h12[[10]], area1[3,]); b3yr <- raster::extract(h12[[10]], area1[1,]); b1yr <- raster::extract(h12[[10]], area1[2,])
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
  geom_boxplot(outlier.alpha = 0)+
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

diff$b3yr = ((diff$b3yr - diff$controle)/diff$controle)*100
diff$b1yr = ((diff$b1yr - diff$controle)/diff$controle)*100
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no PRI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()