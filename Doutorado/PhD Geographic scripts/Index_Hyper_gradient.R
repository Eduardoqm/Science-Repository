#Extrair dados do Landast, Hyperion para os plots A, B e C do meu doutorado
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


#Graphics =======================
ggplot(ndvi, aes(gradient,ndvi, col=plots))+ 
  geom_line(aes(group=plots), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Distancia da borda",y="NDVI")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))



























a <- melt(crt)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yr)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yr)
colnames(b) = c('id', 'data', 'b3yr')

df = as.data.frame(cbind(a, b, c))
df <- df[,c(2, 3, 7, 11)]
df$data = substr(df$data, 18, 25)

#Median NDVI per year
df_md <- df
df_md$data = substr(df_md$data, 1, 4)

df_md = df_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

#Plots
gg <- melt(df_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')

#EVI =======================
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  #geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  #stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="EVI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 1.83, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 1.82, 
           xend = c(8.5, 11.5, 15.5), yend = c(1.75, 1.75, 1.75),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 1.45, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 1.42, ymax = 1.44, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 1.45, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 1.42, ymax = 1.44, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 1.43, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 1.43, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 1.45, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 1.42, ymax = 1.44, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NDVI =======================
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.82, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.815, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.8, 0.8, 0.8),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.705, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.696, ymax = 0.7, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.705, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.696, ymax = 0.7, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.698, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.698, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.705, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.696, ymax = 0.7, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NDWI =======================
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NDWI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.42, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.41, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.39, 0.39, 0.39),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.205, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.197, ymax = 0.19, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.205, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.197, ymax = 0.19, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.194, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.194, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.205, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.197, ymax = 0.19, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
