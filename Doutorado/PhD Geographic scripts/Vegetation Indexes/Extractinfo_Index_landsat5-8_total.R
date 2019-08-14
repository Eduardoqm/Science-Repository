#Extrair dados do Landast para os plots A, B e C do meu doutorado
#Eduardo Q Marques /06/08/2019/

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)

#NDVI ====================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDVI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

ndvi <- stack(list)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
crt <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="Controle_points")
b1yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B1y_pontos")
b3yr <- readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/shapes",layer="B3y_pontos")

#Extrair valor dos pixels
crtdf <- extract(ndvi, crt)
b1yrdf <- extract(ndvi, b1yr)
b3yrdf <- extract(ndvi, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

ndvi = as.data.frame(cbind(a, b, c))
ndvi <- ndvi[,c(2, 3, 6, 9)]
ndvi$data = substr(ndvi$data, 18, 25)

#Median NDVI per year
ndvi_md <- ndvi
ndvi_md$data = substr(ndvi_md$data, 1, 4)

ndvi_md = ndvi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(ndvi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  #geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  #stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="NDVI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.83, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.82, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.8, 0.8, 0.8),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.71, label = "PrÃ©-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.69, ymax = 0.7, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.71, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.69, ymax = 0.7, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.695, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.695, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.71, label = "Periodo de recuperaÃ§Ã£o", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.69, ymax = 0.7, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#EVI ======================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

evi2 <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(evi2, crt)
b1yrdf <- extract(evi2, b1yr)
b3yrdf <- extract(evi2, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

evi2 = as.data.frame(cbind(a, b, c))
evi2 <- evi2[,c(2, 3, 6, 9)]
evi2$data = substr(evi2$data, 18, 25)

#Median NDVI per year
evi2_md <- evi2
evi2_md$data = substr(evi2_md$data, 1, 4)

evi2_md = evi2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(evi2_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
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
  
  annotate("text", x = 2.5, y = 1.45, label = "PrÃ©-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 1.37, ymax = 1.4, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 1.45, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 1.37, ymax = 1.4, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 1.39, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 1.39, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 1.45, label = "Periodo de recuperaÃ§Ã£o", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 1.37, ymax = 1.4, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NDMI ======================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDMI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(index, crt)
b1yrdf <- extract(index, b1yr)
b3yrdf <- extract(index, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

evi2 = as.data.frame(cbind(a, b, c))
evi2 <- evi2[,c(2, 3, 6, 9)]
evi2$data = substr(evi2$data, 18, 25)

#Median NDVI per year
evi2_md <- evi2
evi2_md$data = substr(evi2_md$data, 1, 4)

ndmi_md = evi2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(ndmi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  #geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  #stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="NDMI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.43, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.42, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.4, 0.4, 0.4),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.2, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.18, ymax = 0.16, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.2, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.18, ymax = 0.16, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.17, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.17, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.2, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.18, ymax = 0.16, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#GRND ======================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/GRND", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(index, crt)
b1yrdf <- extract(index, b1yr)
b3yrdf <- extract(index, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

evi2 = as.data.frame(cbind(a, b, c))
evi2 <- evi2[,c(2, 3, 6, 9)]
evi2$data = substr(evi2$data, 18, 25)

#Median NDVI per year
evi2_md <- evi2
evi2_md$data = substr(evi2_md$data, 1, 4)

grnd_md = evi2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(grnd_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  #geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  #stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="GRND")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.21, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.2, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.17, 0.17, 0.17),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.03, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.02, ymax = 0.01, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.03, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.02, ymax = 0.01, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.017, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.017, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.03, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.02, ymax = 0.01, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NDWI ======================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDWI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(index, crt)
b1yrdf <- extract(index, b1yr)
b3yrdf <- extract(index, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

evi2 = as.data.frame(cbind(a, b, c))
evi2 <- evi2[,c(2, 3, 6, 9)]
evi2$data = substr(evi2$data, 18, 25)

#Median NDVI per year
evi2_md <- evi2
evi2_md$data = substr(evi2_md$data, 1, 4)

ndwi_md = evi2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(ndwi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  #geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  #stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="NDWI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.6, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.59, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.57, 0.57, 0.57),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.43, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.42, ymax = 0.41, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.43, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.42, ymax = 0.41, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.415, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.415, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.43, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.42, ymax = 0.41, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
