####################################
# Time Series of Area-1 field data #
# (Hyper and Lansat + Biomass) V4  #
# Eduardo Q Marques 20-11-2021     #
####################################

library(tidyverse)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)


#Littererfall ---------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/LAI e Liteira")

litter <- read_csv("1_master_liteira_area_1_jun2020.csv")
litter$date3 = as.Date(litter$date2,"%d/%m/%Y")

#Modify some elements and calculate ha-1 year-1 -----------------------------------------
litter.sub <- litter%>%
  arrange(plot, pont, date3)%>%
  group_by(plot, pont)%>%
  mutate(L = lag(date3),
         Edge.int = if_else(pont2%in%c('A',"AA","Bo", 'AB','C'),"Borda", "Interior"),
         DaysB = as.numeric(date3 - L, units="days"),
         DaysB = ifelse(is.na(DaysB)|DaysB > 60|DaysB==0, 15, DaysB),
         DatesF = date3 - DaysB/2,
         #Treat=as.factor(plot,labels=c("Control", "B3yr", "B1yr")),
         Value = (weight_1/DaysB)*365*(1/0.48)/(100),
         # Value = (weight_1/DaysB)*365*(1/0.64)/(100),
         Value = ifelse(date3%in%c(ymd("2004-08-10"),
                                   ymd("2004-09-30")) & plot!="A", NA, Value))%>%
  ungroup()%>%
  mutate(Treat = ifelse(plot=='A', 'Controle',
                        ifelse(plot=='B', 'B3yr', 'B1yr')),
         Edge.int = ifelse(pont2%in%c('A','AA','AB', 'B','Bo','C'),'Borda','Interior'))%>%
  select(Treat, pont,Edge.int, date, DatesF, Value)%>%
  mutate(Var='Litterfall')%>%
  filter(Value < 20, Value >=0)

litter.sub2 = litter.sub[,c(1,3,5,6)]
litter.sub2$DatesF = substr(litter.sub2$DatesF, 1,4)
colnames(litter.sub2) = c("Tratamento","Dist","Ano","Valor")
litter.sub2$Ano = as.numeric(litter.sub2$Ano)
litter.sub2 = litter.sub2 %>% filter(Ano < 2020)

#Biomass =================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Biomassa")

bms = read.csv("Biomass_mgha.csv", sep = ",")
colnames(bms) = c("Tratamento", "Dist", "Ano", "Biomass")

#LAI =====================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")

#Modify some elements -------------------------------------------------------------------
lai$parcela = as.character(lai$parcela)
lai$dist = as.character(lai$dist)
lai$parcela[lai$parcela == "control"] <- c("Controle")
lai$parcela[lai$parcela == "b3yr"] <- c("B3yr")
lai$parcela[lai$parcela == "b1yr"] <- c("B1yr")
lai$dist[lai$dist == "borda"] <- c("Borda")
lai$dist[lai$dist == "nucleo"] <- c("Interior")
lai = na.omit(lai)
lai$year = as.character(lai$year)
colnames(lai)[3:5] = c("LAI", "Ano", "Tratamento")

#Vegetation Indices ======================================================================
hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify and Filter Landsat data ==========================================================
land$year = substr(land$year, 1,4)
land$year = as.numeric(land$year)
land$index = as.character(land$index)
land$treat = as.character(land$treat)

land$index[land$index == "grnd"] <- c("GRND")
land$treat[land$treat == "control"] <- c("Controle")
land$treat[land$treat == "b3yr"] <- c("B3yr")
land$treat[land$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean and select same years of Hyperion
land = land %>% 
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value)) %>% 
  filter(year >= 2004) %>% 
  filter(index == "GRND")

#Edge - Core separation
diffy = min(land$y) - max(land$y)
land$dist = ((max(land$y) - land$y)/diffy)*1000
land$dist = abs(land$dist)
summary(land$dist)

land$dist2 = c("a")
land$dist2[land$dist <= 250] = c("Borda")
land$dist2[land$dist > 250] = c("Interior")

colnames(land) = c("x","y","Indice","Ano","Tratamento","Valor","Dist","Dist2")

#Modify and Filter Hyperion data ==========================================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)

hy = hy %>% filter(index == "psri")
hy$index[hy$index == "psri"] <- c("PSRI")
hy$treat[hy$treat == "control"] <- c("Controle")
hy$treat[hy$treat == "b3yr"] <- c("B3yr")
hy$treat[hy$treat == "b1yr"] <- c("B1yr")

#Edge - Core separation
diffy = min(hy$y) - max(hy$y)
hy$dist = ((max(hy$y) - hy$y)/diffy)*1000
hy$dist = abs(hy$dist)
summary(hy$dist)

hy$dist2 = c("a")
hy$dist2[hy$dist <= 250] = c("Borda")
hy$dist2[hy$dist > 250] = c("Interior")

colnames(hy) = c("id1","id2","x","y","layer","Indice","Ano","Valor","Tratamento","Dist","Dist2")

#Join VIs and field data =================================================================
land = land %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")
land = land[,c(-1,-2)]

hy = hy %>% 
  na.omit() %>% 
  #filter(Valor < 0.3) %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")

lai$Indice = c("LAI")
lai$Ano = as.numeric(lai$Ano)
lai = lai %>% 
  select("Indice","Ano","Tratamento","LAI","dist")

litter.sub2$Indice = c("Liteira")
litter.sub2$Ano = as.numeric(litter.sub2$Ano)
litter = litter.sub2 %>% 
  select("Indice","Ano","Tratamento","Valor","Dist")

bms$Indice = c("Biomass")
bms = bms[,c(5,3,1,4,2)]

colnames(land) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(hy) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(lai) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(litter) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(bms) = c("Variavel","Ano","Tratamento","Valor","Dist")


df = full_join(land, hy)
df = full_join(df, lai)
df = full_join(df, litter)
df = full_join(df, bms)
df$Ano2 = as.character(df$Ano)

#Plot data =================================================================================
eqm = c("orange", "red", "blue") #My color palette
#Smooth time series
ggplot(df, aes(x=Ano2, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = "2004", linetype = "dashed")+
  geom_vline(xintercept = "2011", linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_grid(rows = vars(Variavel), cols = vars(Dist), scales = "free")+
  scale_color_manual(values = eqm)+
  #scale_fill_manual(values = eqm)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = NULL, x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14), legend.position = "none",
        axis.text.x = element_blank(), strip.text = element_blank())
  

#Smooth per Variable
a = df %>% 
  filter(Variavel == "LAI") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = NULL)+ #"LAI (m² m-²)"
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_blank(), legend.position = "none",
        strip.text = element_blank())

b = df %>% 
  filter(Variavel == "Liteira") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = NULL)+ #"Mg Ano-¹"
  theme(text = element_text(family = "Times New Roman", size = 14), legend.position = "none",
        axis.text.x = element_blank(), strip.text = element_blank())

c = df %>% 
  filter(Variavel == "GRND") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14), legend.position = "none",
        axis.text.x = element_blank(), strip.text = element_blank())

d = df %>% 
  filter(Variavel == "PSRI") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1,
              method = "lm", formula = y ~ poly(x, 5))+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  #stat_summary(geom="line", fun.data="mean_cl_boot",
  #          size = 1, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist, drop = F)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14),
        strip.text = element_blank(), legend.position = "none")

e = df %>% 
  filter(Variavel == "Biomass") %>% 
  ggplot(aes(x=Ano, y=Valor, color = Tratamento))+
  geom_smooth(aes(group=Tratamento), alpha = 0.3, size = 1)+
  geom_vline(xintercept = 2004, linetype = "dashed")+
  geom_vline(xintercept = 2011, linetype = "dashed")+
  stat_summary(geom="point", fun.data="mean_cl_boot",
               size = 2, alpha = 0.7, aes(group=Tratamento, shape = Tratamento))+
  facet_wrap(~Dist)+
  scale_color_manual(values = eqm)+
  xlim(2004, 2020)+
  theme_minimal()+
  labs(y = NULL, x = NULL)+ #"Mg Ano-¹"
  theme(text = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_blank())

#ggsave(filename = "LAI.png", plot = a,
 #   path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 7, units =  "cm", dpi = 300)

#ggsave(filename = "Liteira.png", plot = b,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 6.5, units =  "cm", dpi = 300)

#ggsave(filename = "GRND.png", plot = c,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 6.5, units =  "cm", dpi = 300)

#ggsave(filename = "PSRI.png", plot = d,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 7, units =  "cm", dpi = 300)

#Join plots
smt = ggarrange(e,a,b,c,d,
          common.legend = TRUE,
          legend="left",
          ncol = 1, nrow = 5,
          widths = 50)
smt

#ggsave(filename = "Smooth_Field and VIs.png", plot = smt,
 #    path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 14, height = 15, units =  "cm", dpi = 300)





