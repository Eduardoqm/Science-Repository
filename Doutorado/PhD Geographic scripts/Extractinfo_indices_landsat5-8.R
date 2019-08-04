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

ndvi = as.data.frame(cbind(a, b, c))
ndvi2 <- ndvi[,c(2, 3, 6, 9)]
ndvi2$data = substr(ndvi2$data, 18, 25)

gg <- melt(ndvi2, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="NDVI", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))

#Median NDVI per year
ndvi_md <- ndvi2
ndvi_md$data = substr(ndvi_md$data, 1, 4)

ndvi_md = ndvi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))


gg <- melt(ndvi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.7, ymax = 0.69, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.7, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.7, label = "x", colour = "yellow")+
  labs(title="NDVI", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))


#EVI2 ===========================================================================================================
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
evi2_2 <- evi2[,c(2, 3, 6, 9)]
evi2_2$data = substr(evi2_2$data, 18, 25)

gg <- melt(evi2_2, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="EVI2", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))

#Median EVI2 per year
evi2_md <- evi2_2
evi2_md$data = substr(evi2_md$data, 1, 4)

evi2_md = evi2_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(evi2_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  annotate("rect", xmin = 5, xmax = 12, ymin = 1.4, ymax = 1.43, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 1.43, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 1.43, label = "x", colour = "yellow")+
  labs(title="EVI2", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))

#NDGI ===========================================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/NDGI", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

ndgi <- stack(list)

#Extrair valor dos pixels
crtdf <- extract(ndgi, crt)
b1yrdf <- extract(ndgi, b1yr)
b3yrdf <- extract(ndgi, b3yr)

a <- melt(crtdf)
colnames(a) = c('id', 'data', 'controle')
c <- melt(b1yrdf)
colnames(c) = c('id', 'data', 'b1yr')
b <- melt(b3yrdf)
colnames(b) = c('id', 'data', 'b3yr')

ndgi = as.data.frame(cbind(a, b, c))
ndgi <- ndgi[,c(2, 3, 6, 9)]
ndgi$data = substr(ndgi$data, 18, 25)

gg <- melt(ndgi, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="GRND", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))


#Median EVI2 per year
ndgi_md <- ndgi
ndgi_md$data = substr(ndgi_md$data, 1, 4)

ndgi_md = ndgi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(ndgi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  labs(title="GRND", fill= "Plot",x="Data",y="Index")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.02, ymax = 0, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.015, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.015, label = "x", colour = "yellow")+
  theme(axis.text.x = element_text(angle = 90))

#NDMI ===========================================================================================================
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

ndmi = as.data.frame(cbind(a, b, c))
ndmi <- ndmi[,c(2, 3, 6, 9)]
ndmi$data = substr(ndmi$data, 18, 25)

gg <- melt(ndmi, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="MSI", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))


#Median EVI2 per year
ndmi_md <- ndmi
ndmi_md$data = substr(ndmi_md$data, 1, 4)

ndmi_md = ndmi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(ndmi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.17, ymax = 0.19, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.185, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.185, label = "x", colour = "yellow")+
  labs(title="MSI", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))

#SWND ===========================================================================================================
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Tanguro Indices/SWND", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

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

ndmi = as.data.frame(cbind(a, b, c))
ndmi <- ndmi[,c(2, 3, 6, 9)]
ndmi$data = substr(ndmi$data, 18, 25)

gg <- melt(ndmi, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_boxplot()+
  labs(title="SWND?", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))


#Median EVI2 per year
ndmi_md <- ndmi
ndmi_md$data = substr(ndmi_md$data, 1, 4)

ndmi_md = ndmi_md %>%
  group_by(data) %>% 
  summarise(controle = median(controle),
            b3yr = median(b3yr),
            b1yr = median(b1yr))

gg <- melt(ndmi_md, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela))+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.369, ymax = 0.375, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.374, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.374, label = "x", colour = "yellow")+
  labs(title="SWND?", fill= "Plot",x="Data",y="Index")+
  theme(axis.text.x = element_text(angle = 90))
