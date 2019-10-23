#Extrair dados do Hyperion pela distancia da borda
#Eduardo Q Marques /29/08/2019/

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#Temporal scale====================================================================================
#Landsat -- obs: Just change de folder name to chande the index and choose the graphic
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/Hyperion/2011", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes/Amostra_poly",layer="Grid_ABC_100m")
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
#EVI =====================
a = bloco_a %>% 
  filter(index == 'evi.2011')
a$index[a$index == 'evi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'evi')

b = bloco_b %>% 
  filter(index == 'evi.2011')
b$index[b$index == 'evi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'evi')

c = bloco_c %>% 
  filter(index == 'evi.2011')
c$index[c$index == 'evi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'evi')

evi = rbind(a,b,c)

#EVI2 ======================
a = bloco_a %>% 
  filter(index == 'evi2.2011')
a$index[a$index == 'evi2.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'evi2')

b = bloco_b %>% 
  filter(index == 'evi2.2011')
b$index[b$index == 'evi2.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'evi2')

c = bloco_c %>% 
  filter(index == 'evi2.2011')
c$index[c$index == 'evi2.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'evi2')

evi2 = rbind(a,b,c)

#NDVI===================
a = bloco_a %>% 
  filter(index == 'ndvi.2011')
a$index[a$index == 'ndvi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'ndvi')

b = bloco_b %>% 
  filter(index == 'ndvi.2011')
b$index[b$index == 'ndvi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'ndvi')

c = bloco_c %>% 
  filter(index == 'ndvi.2011')
c$index[c$index == 'ndvi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'ndvi')

ndvi = rbind(a,b,c)

#MSI===================
a = bloco_a %>% 
  filter(index == 'msi.2011')
a$index[a$index == 'msi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'msi')

b = bloco_b %>% 
  filter(index == 'msi.2011')
b$index[b$index == 'msi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'msi')

c = bloco_c %>% 
  filter(index == 'msi.2011')
c$index[c$index == 'msi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'msi')
  
msi = rbind(a,b,c)


#WBI ===============
a = bloco_a %>% 
  filter(index == 'wbi.2011')
a$index[a$index == 'wbi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'wbi')

b = bloco_b %>% 
  filter(index == 'wbi.2011')
b$index[b$index == 'wbi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'wbi')

c = bloco_c %>% 
  filter(index == 'wbi.2011')
c$index[c$index == 'wbi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'wbi')

wbi = rbind(a,b,c)

#VIG ================
a = bloco_a %>% 
  filter(index == 'vig.2011')
a$index[a$index == 'vig.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'vig')

b = bloco_b %>% 
  filter(index == 'vig.2011')
b$index[b$index == 'vig.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'vig')

c = bloco_c %>% 
  filter(index == 'vig.2011')
c$index[c$index == 'vig.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'vig')

vig = rbind(a,b,c)

#VARI ===============
a = bloco_a %>% 
  filter(index == 'vari.2011')
a$index[a$index == 'vari.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'vari')

b = bloco_b %>% 
  filter(index == 'vari.2011')
b$index[b$index == 'vari.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'vari')

c = bloco_c %>% 
  filter(index == 'vari.2011')
c$index[c$index == 'vari.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'vari')

vari = rbind(a,b,c)

#SIPI ===============
a = bloco_a %>% 
  filter(index == 'sipi.2011')
a$index[a$index == 'sipi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'sipi')

b = bloco_b %>% 
  filter(index == 'sipi.2011')
b$index[b$index == 'sipi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'sipi')

c = bloco_c %>% 
  filter(index == 'sipi.2011')
c$index[c$index == 'sipi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'sipi')

sipi = rbind(a,b,c)

#RENDVI =============
a = bloco_a %>% 
  filter(index == 'rendvi.2011')
a$index[a$index == 'rendvi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'rendvi')

b = bloco_b %>% 
  filter(index == 'rendvi.2011')
b$index[b$index == 'rendvi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'rendvi')

c = bloco_c %>% 
  filter(index == 'rendvi.2011')
c$index[c$index == 'rendvi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'rendvi')

rendvi = rbind(a,b,c)

#PSSR ================
a = bloco_a %>% 
  filter(index == 'pssr.2011')
a$index[a$index == 'pssr.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'pssr')

b = bloco_b %>% 
  filter(index == 'pssr.2011')
b$index[b$index == 'pssr.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'pssr')

c = bloco_c %>% 
  filter(index == 'pssr.2011')
c$index[c$index == 'pssr.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'pssr')

pssr = rbind(a,b,c)

#PSRI ===================
a = bloco_a %>% 
  filter(index == 'psri.2011')
a$index[a$index == 'psri.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'psri')

b = bloco_b %>% 
  filter(index == 'psri.2011')
b$index[b$index == 'psri.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'psri')

c = bloco_c %>% 
  filter(index == 'psri.2011')
c$index[c$index == 'psri.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'psri')

psri = rbind(a,b,c)

#PRI ====================
a = bloco_a %>% 
  filter(index == 'pri.2011')
a$index[a$index == 'pri.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'pri')

b = bloco_b %>% 
  filter(index == 'pri.2011')
b$index[b$index == 'pri.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'pri')

c = bloco_c %>% 
  filter(index == 'pri.2011')
c$index[c$index == 'pri.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'pri')

pri = rbind(a,b,c)

#NIRV ===================
a = bloco_a %>% 
  filter(index == 'nirv.2011')
a$index[a$index == 'nirv.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'nirv')

b = bloco_b %>% 
  filter(index == 'nirv.2011')
b$index[b$index == 'nirv.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'nirv')

c = bloco_c %>% 
  filter(index == 'nirv.2011')
c$index[c$index == 'nirv.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'nirv')

nirv = rbind(a,b,c)

#NDWI ====================
a = bloco_a %>% 
  filter(index == 'ndwi.2011')
a$index[a$index == 'ndwi.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'ndwi')

b = bloco_b %>% 
  filter(index == 'ndwi.2011')
b$index[b$index == 'ndwi.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'ndwi')

c = bloco_c %>% 
  filter(index == 'ndwi.2011')
c$index[c$index == 'ndwi.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'ndwi')

ndwi = rbind(a,b,c)

#NDII =====================
a = bloco_a %>% 
  filter(index == 'ndii.2011')
a$index[a$index == 'ndii.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'ndii')

b = bloco_b %>% 
  filter(index == 'ndii.2011')
b$index[b$index == 'ndii.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'ndii')

c = bloco_c %>% 
  filter(index == 'ndii.2011')
c$index[c$index == 'ndii.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'ndii')

ndii = rbind(a,b,c)

#LWVI2 =====================
a = bloco_a %>% 
  filter(index == 'lwvi2.2011')
a$index[a$index == 'lwvi2.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'lwvi2')

b = bloco_b %>% 
  filter(index == 'lwvi2.2011')
b$index[b$index == 'lwvi2.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'lwvi2')

c = bloco_c %>% 
  filter(index == 'lwvi2.2011')
c$index[c$index == 'lwvi2.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'lwvi2')

lwvi2 = rbind(a,b,c)

#ARI ======================
a = bloco_a %>% 
  filter(index == 'ari.2011')
a$index[a$index == 'ari.2011'] <- 'control'
colnames(a) = c('gradient', 'plots', 'ari')

b = bloco_b %>% 
  filter(index == 'ari.2011')
b$index[b$index == 'ari.2011'] <- 'b1yr'
colnames(b) = c('gradient', 'plots', 'ari')

c = bloco_c %>% 
  filter(index == 'ari.2011')
c$index[c$index == 'ari.2011'] <- 'b3yr'
colnames(c) = c('gradient', 'plots', 'ari')

ari = rbind(a,b,c)
#Graphics =======================
ggplot(ndii, aes(gradient,ndii, col=plots))+ 
  geom_line(aes(group=plots), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Distancia da borda (m)",y="NDII")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))



