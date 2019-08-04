library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)

#NDVI =========================================================================================================
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

#Extrair porcentagem
#c$b1yr <- (c$b1yr*100)/a$controle
#b$b3yr <- (b$b3yr*100)/a$controle

#Extrair diferenca
c$b1yr <- (c$b1yr - a$controle)*100
b$b3yr <- (b$b3yr - a$controle)*100


ndvi = as.data.frame(cbind(b, c))
ndvi <- ndvi[,c(2, 3, 6)]
ndvi$data = substr(ndvi$data, 18, 25)

#Median NDVI per year
ndvi_md <- ndvi
ndvi_md$data = substr(ndvi_md$data, 1, 4)

ndvi_md = ndvi_md %>%
  group_by(data) %>% 
  summarise(b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(ndvi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NDVI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 1.5, label = "El Niño")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 1, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -7.5, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -7.5, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -8.1, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -8.1, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -7.5, label = "Período de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9, 0.5))
  

#EVI =========================================================================================================
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

#Extrair diferenca
c$b1yr <- (c$b1yr - a$controle)*100
b$b3yr <- (b$b3yr - a$controle)*100


evi2 = as.data.frame(cbind(b, c))
evi2 <- evi2[,c(2, 3, 6)]
evi2$data = substr(evi2$data, 18, 25)

#Median NDVI per year
evi2_md <- evi2
evi2_md$data = substr(evi2_md$data, 1, 4)

evi2_md = evi2_md %>%
  group_by(data) %>% 
  summarise(b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(evi2_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no EVI2)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 3, label = "El Niño")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 2, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -21.5, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -23, ymax = -24.5, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -21.5, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -23, ymax = -24.5, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -23.3, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -23.3, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -21.5, label = "Período de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -23, ymax = -24.5, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9, 0.5))


#NDMI =========================================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDMI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

ndmi <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(ndmi, crt)
b1yrdf <- extract(ndmi, b1yr)
b3yrdf <- extract(ndmi, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

#Extrair diferenca
c$b1yr <- (c$b1yr - a$controle)*100
b$b3yr <- (b$b3yr - a$controle)*100


ndmi = as.data.frame(cbind(b, c))
ndmi <- ndmi[,c(2, 3, 6)]
ndmi$data = substr(ndmi$data, 18, 25)

#Median NDVI per year
ndmi_md <- ndmi
ndmi_md$data = substr(ndmi_md$data, 1, 4)

ndmi_md = ndmi_md %>%
  group_by(data) %>% 
  summarise(b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(ndmi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NDMI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 3, label = "El Niño")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 2, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -12, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -14, ymax = -15, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -12, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -14, ymax = -15, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -14.3, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -14.3, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -12, label = "Período de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -14, ymax = -15, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9, 0.5))


#NBRI =========================================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NBRI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

nbri <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(nbri, crt)
b1yrdf <- extract(nbri, b1yr)
b3yrdf <- extract(nbri, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

#Extrair diferenca
c$b1yr <- (c$b1yr - a$controle)*100
b$b3yr <- (b$b3yr - a$controle)*100


nbri = as.data.frame(cbind(b, c))
nbri <- nbri[,c(2, 3, 6)]
nbri$data = substr(nbri$data, 18, 25)

#Median per year
nbri_md <- nbri
nbri_md$data = substr(nbri_md$data, 1, 4)

nbri_md = nbri_md %>%
  group_by(data) %>% 
  summarise(b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(nbri_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NBRI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 3, label = "El Niño")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 2, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -12, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -13, ymax = -14, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -12, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -13, ymax = -14, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -13.3, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -13.3, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -12, label = "Período de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -13, ymax = -14, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9, 0.5))


#NPCRI =========================================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NPCRI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

npcri <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(npcri, crt)
b1yrdf <- extract(npcri, b1yr)
b3yrdf <- extract(npcri, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

#Extrair diferenca
c$b1yr <- (c$b1yr - a$controle)*100
b$b3yr <- (b$b3yr - a$controle)*100

npcri = as.data.frame(cbind(b, c))
npcri <- npcri[,c(2, 3, 6)]
npcri$data = substr(npcri$data, 18, 25)

#Median per year
npcri_md <- npcri
npcri_md$data = substr(npcri_md$data, 1, 4)

npcri_md = npcri_md %>%
  group_by(data) %>% 
  summarise(b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(npcri_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1, linetype = "dashed")+
  geom_point()+
  stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NPCRI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 6.5, label = "El Niño")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 5.5, 
           xend = c(8.5, 11.5, 15.5), yend = 4.5,
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -2, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -2.5, ymax = -3, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -2, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -2.5, ymax = -3, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -2.7, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -2.7, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -2, label = "Período de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -2.5, ymax = -3, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90),legend.position = c(0.9, 0.8))
