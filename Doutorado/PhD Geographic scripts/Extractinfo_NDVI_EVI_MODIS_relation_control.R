library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#NDVI ====================================================================================
a = read.csv("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/MODIS/plot_A.csv")

b = read.csv("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/MODIS/plot_B.csv")

c = read.csv("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/MODIS/plot_C.csv")


as.numeric(gsub(",", "", a$NDVI)); as.integer(gsub(",", "", a$EVI))
a = a %>% 
  separate(col = "system.time_start", c("dia", "ano"), sep = ',')
a <- a[,c(2, 5, 6)]
colnames(a) = c("ano", "EVI_a", "NDVI_a")

as.numeric(gsub(",", "", b$NDVI)); as.integer(gsub(",", "", b$EVI))
b = b %>% 
  separate(col = "system.time_start", c("dia", "ano"), sep = ',')
b <- b[,c(2, 5, 6)]
colnames(b) = c("ano", "EVI_b", "NDVI_b")

as.numeric(gsub(",", "", c$NDVI)); as.integer(gsub(",", "", c$EVI))
c = c %>% 
  separate(col = "system.time_start", c("dia", "ano"), sep = ',')
c <- c[,c(2, 5, 6)]
colnames(c) = c("ano", "EVI_c", "NDVI_c")

ndvi = as.data.frame(cbind(a, b, c))
ndvi <- ndvi[,c(1,2,3,5,6,8,9)]


#Median NDVI per year
ndvi_md = ndvi %>%
  na.omit() %>%
  group_by(ano) %>% 
  summarise(EVI_a = median(as.numeric(EVI_a)),
            NDVI_a = median(as.numeric(NDVI_a)),
            EVI_b = median(as.numeric(EVI_b)),
            NDVI_b = median(as.numeric(NDVI_b)),
            EVI_c = median(as.numeric(EVI_c)),
            NDVI_c = median(as.numeric(NDVI_c)))

ndvi = ndvi_md[,c(1,3,5,7)]
colnames(ndvi) = c("data", "controle", "b3yr", "b1yr")
ndvi$controle = ndvi$controle/100
ndvi$b3yr = ndvi$b3yr/100
ndvi$b1yr = ndvi$b1yr/100


#Extrair diferenca
ndvi$b1yr <- (ndvi$b1yr - ndvi$controle)*100
ndvi$b3yr <- (ndvi$b3yr - ndvi$controle)*100

ndvi = ndvi[,c(1,3,4)]



gg <- melt(ndvi, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
 # stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no NDVI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 90, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 80, 
           xend = c(8.5, 11.5, 15.5), yend = c(25, 25, 25),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -50, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -70, ymax = -60, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -50, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -70, ymax = -60, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -65, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -65, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -50, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -70, ymax = -60, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
  

#EVI =======================================================================================
evi = ndvi_md[,c(1,2,4,6)]
colnames(evi) = c("data", "controle", "b3yr", "b1yr")
evi$controle = evi$controle/100
evi$b3yr = evi$b3yr/100
evi$b1yr = evi$b1yr/100

#Extrair diferenca
evi$b1yr <- (evi$b1yr - evi$controle)*100
evi$b3yr <- (evi$b3yr - evi$controle)*100

evi = evi[,c(1,3,4)]


gg <- melt(evi, id.vars="data")
colnames(gg) = c('Data', 'Parcela', 'Refl')
ggplot(gg, aes(Data,Refl, col=Parcela))+ 
  geom_line(aes(group=Parcela), size = 1)+
  geom_point()+
  # stat_smooth(aes(group=Parcela), method = "loess", formula = y ~ x, size = 0.5, alpha = 0.15)+
  labs(fill= "Plot",x="Ano",y="Fogo - Controle (% diferença no EVI)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(8.5, 11.5, 15.5), y = 90, label = "Seca")+
  annotate("segment", x = c(8.5, 11.5, 15.5), y = 80, 
           xend = c(8.5, 11.5, 15.5), yend = c(25, 25, 25),
           arrow = arrow(angle = 20, length = unit(2, "mm"), type = "closed"))+
  
  annotate("text", x = 2.5, y = -70, label = "Pré-fogo", colour = "darkgreen")+
  annotate("rect", xmin = 1, xmax = 5, ymin = -90, ymax = -80, alpha = 0.9, fill = "darkgreen")+
  
  annotate("text", x = 8.5, y = -70, label = "Experimento de fogo", colour = "red")+
  annotate("rect", xmin = 5, xmax = 12, ymin = -90, ymax = -80, alpha = 0.9, fill = "red")+
  annotate("text", x = c(5.5, 8.5, 11.5), y = -84, label = "xx", colour = "yellow")+
  annotate("text", x = c(6.5, 7.5, 10.5), y = -84, label = "x", colour = "yellow")+
  
  annotate("text", x = 15.5, y = -70, label = "Periodo de recuperação", colour = "darkblue")+
  annotate("rect", xmin = 12, xmax = 19, ymin = -90, ymax = -80, alpha = 0.9, fill = "darkblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
