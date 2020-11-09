#Extrair dados do Landast, Hyperion para os plots A, B e C do meu doutorado
#Eduardo Q Marques /29/08/2019/

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(dplyr)

#Temporal scale================================================================================
#Landsat -- obs: Just change de folder name to chande the index and choose the graphic
list <- list.files(path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Tanguro Indices/Landsat/EVI2", pattern = ".tif$", full.names=TRUE,recursive=TRUE)

index <- stack(list)

#Abrir os shapes para amostrar pixels (100 pontos por shape)
area1 <-readOGR(dsn = "C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/shapes",layer="Polygon_A_B_C")
area1 = spTransform(area1, crs(index))

#Extrair valor dos pixels
crt <- extract(index, area1[1,])#PLot A
b3yr <- extract(index, area1[2,])#PLot B
b1yr <- extract(index, area1[3,])#PLot c

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
  geom_smooth(aes(group=Parcela))+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="EVI")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="EVI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.5, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.495, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.48, 0.48, 0.48),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.33, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.32, ymax = 0.31, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.33, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.32, ymax = 0.31, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.315, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.315, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.33, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.32, ymax = 0.31, alpha = 0.9, fill = "darkblue")+
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

#NBRI =======================
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="NBRI")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 0.75, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 0.74, 
           xend = c(8.5, 11.5, 15.5), yend = c(0.72, 0.72, 0.72),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = 0.53, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = 0.52, ymax = 0.51, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = 0.53, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = 0.52, ymax = 0.51, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = 0.516, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = 0.516, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = 0.53, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = 0.52, ymax = 0.51, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#Extract difference ====================
dif = df_md %>% 
  mutate(b3_dif = (b3yr - controle)*100) %>%
  mutate(b1_dif = (b1yr - controle)*100)
  
colnames(dif) = c('data', 'a', 'b', 'c', 'b3yr', 'b1yr')
dif <- dif[,c(1, 5, 6)]

#Graphic
#EVI ==================
gg <- melt(dif, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no EVI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 4, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 3, 
           xend = c(8.5, 11.5, 15.5), yend = c(1.5, 1.5, 1.5),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -10, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -10.5, ymax = -11, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -10, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -10.5, ymax = -11, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -10.7, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -10.7, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -10, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -10.5, ymax = -11, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#NDVI ==================
gg <- melt(dif, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NDVI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 1.5, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 1, 
  xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
  arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -7.7, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -7.7, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -8.1, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -8.1, label = "x", colour = "yellow")+

  annotate("text", x = 15.5, y = -7.7, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -8, ymax = -8.5, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NDwI ==================
gg <- melt(dif, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NDWI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 1.5, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 1, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -14, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -14, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -14.7, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -14.7, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -14, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#NBRI ==================
gg <- melt(dif, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NBRI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 1.5, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 1, 
           xend = c(8.5, 11.5, 15.5), yend = c(0, 0, 0),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -14, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -14, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -14.7, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -14.7, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -14, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -14.5, ymax = -15, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
