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
crt <- raster::extract(h04[[13]], area1[3,]); b3yr <- raster::extract(h04[[13]], area1[1,]); b1yr <- raster::extract(h04[[13]], area1[2,])
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

crt <- raster::extract(h05[[13]], area1[3,]); b3yr <- raster::extract(h05[[13]], area1[1,]); b1yr <- raster::extract(h05[[13]], area1[2,])
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

crt <- raster::extract(h06[[13]], area1[3,]); b3yr <- raster::extract(h06[[13]], area1[1,]); b1yr <- raster::extract(h06[[13]], area1[2,])
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

crt <- raster::extract(h08[[13]], area1[3,]); b3yr <- raster::extract(h08[[13]], area1[1,]); b1yr <- raster::extract(h08[[13]], area1[2,])
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

crt <- raster::extract(h10[[13]], area1[3,]); b3yr <- raster::extract(h10[[13]], area1[1,]); b1yr <- raster::extract(h10[[13]], area1[2,])
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

crt <- raster::extract(h11[[13]], area1[3,]); b3yr <- raster::extract(h11[[13]], area1[1,]); b1yr <- raster::extract(h11[[13]], area1[2,])
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

crt <- raster::extract(h12[[13]], area1[3,]); b3yr <- raster::extract(h12[[13]], area1[1,]); b1yr <- raster::extract(h12[[13]], area1[2,])
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
  geom_boxplot(outlier.alpha = 0)+
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

diff$b3yr = ((diff$b3yr - diff$controle)/diff$controle)*100
diff$b1yr = ((diff$b1yr - diff$controle)/diff$controle)*100
diff = diff[,c(1,2)]

gg = melt(diff)
colnames(gg) = c("parcela","index")
gg$data = c(2004, 2005, 2006, 2008, 2010, 2011, 2012, 2004, 2005, 2006, 2008, 2010, 2011, 2012)


ggplot(gg, aes(data,index, col=parcela))+ 
  geom_line(aes(group=parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% taxa de mudança no RENDVI)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()