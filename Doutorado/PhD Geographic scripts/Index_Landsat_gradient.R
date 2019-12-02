#Extrair dados do Landast, Hyperion para os plots A, B e C do meu doutorado
#Eduardo Q Marques /29/08/2019/

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#Temporal scale=============================================================================
#Landsat -- obs: Just change de folder name to chande the index and choose the graphic
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Landsat/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
#area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="Grid_ABC_100m")
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Landsat",layer="Grid_ABC_100m")
area1 = spTransform(area1, crs(index))

#Extract data sequence
#Control
crt_df = c(1)
crt_seq = seq(3,30, by=3)

for (x in crt_seq) {
  plot(index[[12]], main = 'EXTRACTION PROGRESS - CONTROL')
  plot(area1[x,], add=TRUE)
  crt <- raster::extract(index, area1[x,])
  b <- melt(crt)
  colnames(b) = c('id', 'index', 'crt', 'l1')
  b$gradient = c(x)
  crt_df = as.data.frame(rbind(crt_df, b))
}

#Mean per gradient
df <- crt_df
df <- df[,c(2, 3, 5)]

bloco_a = df %>%
  na.omit() %>% 
  unite(col = "gradient2", c("gradient", "index"), sep = '-') %>% 
  group_by(gradient2) %>%
  summarise(b3yr = mean(crt)) %>% 
  separate(col = "gradient2", c("gradient", "index"), sep = '-')


#B3yr
b3yr_df = c(1)
b3yr_seq = seq(2,30, by=3)

for (x in b3yr_seq) {
  plot(index[[12]], main = 'EXTRACTION PROGRESS - B3yr')
  plot(area1[x,], add=TRUE)
  b3yr <- raster::extract(index, area1[x,])
  b <- melt(b3yr)
  colnames(b) = c('id', 'index', 'b3yr', 'l1')
  b$gradient = c(x)
  b3yr_df = as.data.frame(rbind(b3yr_df, b))
}

#Mean per gradient
df <- b3yr_df
df <- df[,c(2, 3, 5)]

bloco_b = df %>%
  na.omit() %>% 
  unite(col = "gradient2", c("gradient", "index"), sep = '-') %>% 
  group_by(gradient2) %>%
  summarise(b3yr = mean(b3yr)) %>% 
  separate(col = "gradient2", c("gradient", "index"), sep = '-')


#B1yr
b1yr_df = c(1)
b1yr_seq = seq(1,30, by=3)

for (x in b1yr_seq) {
  plot(index[[12]], main = 'EXTRACTION PROGRESS - B1yr')
  plot(area1[x,], add=TRUE)
  b1yr <- raster::extract(index, area1[x,])
  b <- melt(b1yr)
  colnames(b) = c('id', 'index', 'b1yr', 'l1')
  b$gradient = c(x)
  b1yr_df = as.data.frame(rbind(b1yr_df, b))
}

#Mean per gradient
df <- b1yr_df
df <- df[,c(2, 3, 5)]

bloco_c = df %>%
  na.omit() %>% 
  unite(col = "gradient2", c("gradient", "index"), sep = '-') %>% 
  group_by(gradient2) %>%
  summarise(b3yr = mean(b1yr)) %>% 
  separate(col = "gradient2", c("gradient", "index"), sep = '-')


#Correct gradient Scales=======
#Control
bloco_a$gradient[bloco_a$gradient == 3] <- 900
bloco_a$gradient[bloco_a$gradient == 6] <- 800
bloco_a$gradient[bloco_a$gradient == 9] <- 700
bloco_a$gradient[bloco_a$gradient == 12] <- 600
bloco_a$gradient[bloco_a$gradient == 15] <- 500
bloco_a$gradient[bloco_a$gradient == 18] <- 400
bloco_a$gradient[bloco_a$gradient == 21] <- 300
bloco_a$gradient[bloco_a$gradient == 24] <- 200
bloco_a$gradient[bloco_a$gradient == 27] <- 100
bloco_a$gradient[bloco_a$gradient == 30] <- 0

#B3yr
bloco_b$gradient[bloco_b$gradient == 2] <- 900
bloco_b$gradient[bloco_b$gradient == 5] <- 800
bloco_b$gradient[bloco_b$gradient == 8] <- 700
bloco_b$gradient[bloco_b$gradient == 11] <- 600
bloco_b$gradient[bloco_b$gradient == 14] <- 500
bloco_b$gradient[bloco_b$gradient == 17] <- 400
bloco_b$gradient[bloco_b$gradient == 20] <- 300
bloco_b$gradient[bloco_b$gradient == 23] <- 200
bloco_b$gradient[bloco_b$gradient == 26] <- 100
bloco_b$gradient[bloco_b$gradient == 29] <- 0

#B1yr
bloco_c$gradient[bloco_c$gradient == 1] <- 900
bloco_c$gradient[bloco_c$gradient == 4] <- 800
bloco_c$gradient[bloco_c$gradient == 7] <- 700
bloco_c$gradient[bloco_c$gradient == 10] <- 600
bloco_c$gradient[bloco_c$gradient == 13] <- 500
bloco_c$gradient[bloco_c$gradient == 16] <- 400
bloco_c$gradient[bloco_c$gradient == 19] <- 300
bloco_c$gradient[bloco_c$gradient == 22] <- 200
bloco_c$gradient[bloco_c$gradient == 25] <- 100
bloco_c$gradient[bloco_c$gradient == 28] <- 0



#Group by index and all plots
#NDVI===================
a = bloco_a %>% 
  filter(index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1')
a$index[a$index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1'] <- 'Control'
colnames(a) = c('gradient', 'plots', 'ndvi')

b = bloco_b %>% 
  filter(index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1')
b$index[b$index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1'] <- 'B1yr'
colnames(b) = c('gradient', 'plots', 'ndvi')

c = bloco_c %>% 
  filter(index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1')
c$index[c$index == 'LT05_L1TP_224069_20040623_20161130_01_T1_sr_band1'] <- 'B3yr'
colnames(c) = c('gradient', 'plots', 'ndvi')

ndvi = rbind(a,b,c)

#Graphics =======================
ggplot(ndvi, aes(gradient,ndvi, col=plots))+ 
  geom_line(aes(group=plots), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Distancia da borda (m)",y="NDVI")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))



