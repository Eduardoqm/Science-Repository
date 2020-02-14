#Linear Model from EVI

library(raster); library(rgdal); library(ggplot2)
library(reshape2); library(dplyr)

#Open data banks
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)
ndvi <- stack(list)

#Shapes edge (100m) and core
a_100 <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="A_100m")
b_100 <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="B_100m")
c_100 <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="C_100m")
d_100 <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="D_100m")

a_core <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="A_core")
b_core <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="B_core")
c_core <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="C_core")
d_core <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="D_core")

#Reproject shapes
proj=projection(ndvi)
a_100=spTransform(a_100,proj); b_100=spTransform(b_100,proj)
c_100=spTransform(c_100,proj); d_100=spTransform(d_100,proj)

a_core=spTransform(a_core,proj); b_core=spTransform(b_core,proj)
c_core=spTransform(c_core,proj); d_core=spTransform(d_core,proj)

#Crop to sample
index_a100=crop(ndvi, a_100); index_b100=crop(ndvi, b_100)
index_c100=crop(ndvi, c_100); index_d100=crop(ndvi, d_100)

index_acore=crop(ndvi, a_core); index_bcore=crop(ndvi, b_core)
index_ccore=crop(ndvi, c_core); index_dcore=crop(ndvi, d_core)

#Extract sample dataframes
index_a100  <-  rasterToPoints(index_a100)
index_a100 <-  data.frame(index_a100)
index_a100 <- index_a100[,c(3:54)]
index_b100  <-  rasterToPoints(index_b100)
index_b100 <-  data.frame(index_b100)
index_b100 <- index_b100[,c(3:54)]
index_c100  <-  rasterToPoints(index_c100)
index_c100 <-  data.frame(index_c100)
index_c100 <- index_c100[,c(3:54)]
index_d100  <-  rasterToPoints(index_d100)
index_d100 <-  data.frame(index_d100)
index_d100 <- index_d100[,c(3:54)]

index_acore  <-  rasterToPoints(index_acore)
index_acore <-  data.frame(index_acore)
index_acore <- index_acore[,c(3:54)]
index_bcore  <-  rasterToPoints(index_bcore)
index_bcore <-  data.frame(index_bcore)
index_bcore <- index_bcore[,c(3:54)]
index_ccore  <-  rasterToPoints(index_ccore)
index_ccore <-  data.frame(index_ccore)
index_ccore <- index_ccore[,c(3:54)]
index_dcore  <-  rasterToPoints(index_dcore)
index_dcore <-  data.frame(index_dcore)
index_dcore <- index_dcore[,c(3:54)]

#Unification dataframes
a <- melt(index_a100)
colnames(a) = c('data', 'controle')
c <- melt(index_c100)
colnames(c) = c('data', 'b1yr')
b <- melt(index_b100)
colnames(b) = c('data', 'b3yr')
d <- melt(index_d100)
colnames(d) = c('data', 'plot_d')
ndvi_100 = as.data.frame(cbind(a, b, c, d))
ndvi_100 <- ndvi_100[,c(1,2,4,6,8)]
ndvi_100$data = substr(ndvi_100$data, 18, 25)

a <- melt(index_acore)
colnames(a) = c('data', 'controle')
c <- melt(index_ccore)
colnames(c) = c('data', 'b1yr')
b <- melt(index_bcore)
colnames(b) = c('data', 'b3yr')
d <- melt(index_dcore)
colnames(d) = c('data', 'plot_d')
ndvi_core = as.data.frame(cbind(a, b, c, d))
ndvi_core <- ndvi_core[,c(1,2,4,6,8)]
ndvi_core$data = substr(ndvi_core$data, 18, 25)

#Extract diference
#ndvi_100$b1yr <- (ndvi_100$b1yr - ndvi_100$controle)*100
#ndvi_100$b3yr <- (ndvi_100$b3yr - ndvi_100$controle)*100
#ndvi_100$plot_d <- (ndvi_100$plot_d - ndvi_100$controle)*100

#ndvi_core$b1yr <- (ndvi_core$b1yr - ndvi_core$controle)*100
#ndvi_core$b3yr <- (ndvi_core$b3yr - ndvi_core$controle)*100
#ndvi_core$plot_d <- (ndvi_core$plot_d - ndvi_core$controle)*100

#Core Linear regression by SPI
ndvi_md <- ndvi_core
ndvi_md$data = substr(ndvi_md$data, 1, 4)

ndvi_md = ndvi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr),
            plot_d = median(plot_d))
ndvi_md2 <- ndvi_md[-c(19),] 

setwd('C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Climate data')
spi <- read.csv("SPI_Area1.csv", header = TRUE, sep = ",")

clima <- read.csv("ERA5_Tanguro_1979_2018.csv", header = TRUE, sep = ",")
View(clima2)

clima2 <- clima[,c(4,13)]
clima2 = clima2 %>% 
  group_by(year) %>% 
  summarise(mcwd = mean(mcwd))
clima2 <- clima2[-c(1:21),]
mcwd <- clima2[-c(13, 20),]


#B1yr
crt_lm<-lm(ndvi_md2$controle~spi$d) #(Y~X)
b3yr_lm<-lm(ndvi_md2$b3yr~spi$d)
b1yr_lm<-lm(ndvi_md2$b1yr~spi$d)
d_lm<-lm(ndvi_md2$plot_d~spi$d)
summary(crt_lm)
summary(b1yr_lm)
summary(b3yr_lm)
summary(d_lm)

#Normality test (valores de p > 0,05 indicam dados normais)
shapiro.test(rstudent(crt_lm)) #shapiro wilk
shapiro.test(rstudent(b3yr_lm))
shapiro.test(rstudent(b1yr_lm))
shapiro.test(rstudent(d_lm))

#Homogenidade residuos
plot(rstudent(crt_lm) ~ fitted(crt_lm), pch = 19)
abline(h = 0, lty = 2)

#Visualization in ggplot
gg = as.data.frame(cbind(ndvi_md2$controle, ndvi_md2$b3yr, ndvi_md2$b1yr, ndvi_md2$plot_d, spi$d))
colnames(gg) = c('control', 'b3yr', 'b1yr', 'plot_d', 'spi')
gg <- melt(gg, id.vars="spi")
colnames(gg) = c('SPI', 'Plot', 'NDVI')

ggplot(gg, aes(SPI,NDVI, col=Plot))+ 
  geom_point()+
  stat_smooth(method = "lm", se=FALSE)+
  theme_minimal()+
  theme(legend.position = 'top')

gg = as.data.frame(cbind(ndvi_md2$controle, ndvi_md2$b3yr, ndvi_md2$b1yr, ndvi_md2$plot_d, mcwd$mcwd))
colnames(gg) = c('control', 'b3yr', 'b1yr', 'plot_d', 'mcwd')
gg <- melt(gg, id.vars="mcwd")
colnames(gg) = c('MCWD', 'Plot', 'NDVI')

ggplot(gg, aes(MCWD,NDVI, col=Plot))+ 
  geom_point()+
  stat_smooth(method = "lm", se=FALSE)+
  theme_minimal()+
  theme(legend.position = 'top')

