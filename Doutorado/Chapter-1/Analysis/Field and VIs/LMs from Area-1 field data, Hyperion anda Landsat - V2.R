############################################
# Linear Models for Area-1 field data - V2 #
# (Hyper and Landsat + Biomass)            #
# Eduardo Q Marques 29-01-2024             #
############################################

library(tidyverse)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(svglite)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

extrafont::loadfonts(device="win") # esse comando rodado antes de liberar o ggplot2 evita alguns erros duarante a mudan?as de fontes de texto nas figuras
windowsFonts(Times=windowsFont("TT Times New Roman"))


#Littererfall ---------------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/LAI e Liteira")

litter <- read_csv("1_master_liteira_area_1_jun2020.csv")
litter$date3 = as.Date(litter$date2,"%d/%m/%Y")

#Modify some elements and calculate ha-1 year-1 -----------------------------------------
litter.sub <- litter%>%
  arrange(plot, pont, date3)%>%
  group_by(plot, pont)%>%
  mutate(L = lag(date3),
         Edge.int = if_else(pont2%in%c('A',"AA","Bo", 'AB','C'),"Edge", "Interior"),
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
         Edge.int = ifelse(pont2%in%c('A','AA','AB', 'B','Bo','C'),'Edge','Interior'))%>%
  select(Treat, pont,Edge.int, date, DatesF, Value)%>%
  mutate(Var='Litterfall')%>%
  filter(Value < 20, Value >=0)

litter.sub2 = litter.sub[,c(1,3,5,6)]
litter.sub2$DatesF = substr(litter.sub2$DatesF, 1,4)
colnames(litter.sub2) = c("Tratamento","Dist","Ano","Valor")
litter.sub2$Ano = as.numeric(litter.sub2$Ano)
litter.sub2 = litter.sub2 %>% filter(Ano < 2020)

#Biomass =================================================================================
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Biomassa")

bms = read.csv("Biomass_mgha.csv", sep = ",")
colnames(bms) = c("Tratamento", "Dist", "Ano", "Biomass")
bms$Dist[bms$Dist == "Borda"] = c("Edge")

#LAI =====================================================================================
setwd("C:/Users/Workshop/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")
lai$dist[lai$dist == "borda"] = c("Edge")

#Modify some elements -------------------------------------------------------------------
lai$parcela = as.character(lai$parcela)
lai$dist = as.character(lai$dist)
lai$parcela[lai$parcela == "control"] <- c("Controle")
lai$parcela[lai$parcela == "b3yr"] <- c("B3yr")
lai$parcela[lai$parcela == "b1yr"] <- c("B1yr")
lai$dist[lai$dist == "Edge"] <- c("Edge")
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

land$index[land$index == "evi2"] <- c("EVI")
land$index[land$index == "ndii"] <- c("NDII")
land$index[land$index == "grnd"] <- c("GRND")
land$index[land$index == "nbr"] <- c("NBR")

land$treat[land$treat == "control"] <- c("Controle")
land$treat[land$treat == "b3yr"] <- c("B3yr")
land$treat[land$treat == "b1yr"] <- c("B1yr")

#Resume repeat years with mean and select same years of Hyperion
land = land %>% 
  group_by(x, y, index, year, treat) %>%
  filter(index %in% c("EVI","NDII","GRND","NBR")) %>% 
  summarise(value = mean(value)) %>% 
  filter(year >= 2004)

#Edge - Core separation
diffy = min(land$y) - max(land$y)
land$dist = ((max(land$y) - land$y)/diffy)*1000
land$dist = abs(land$dist)
summary(land$dist)

land$dist2 = c("a")
land$dist2[land$dist <= 250] = c("Edge")
land$dist2[land$dist > 250] = c("Interior")

colnames(land) = c("x","y","Indice","Ano","Tratamento","Valor","Dist","Dist2")

#Modify and Filter Hyperion data ==========================================================
hy$year = as.numeric(hy$year)
hy$index = as.character(hy$index)
hy$treat = as.character(hy$treat)

hy$index[hy$index == "vig"] <- c("VIG")
hy$index[hy$index == "vari"] <- c("VARI")
hy$index[hy$index == "msi"] <- c("MSI")
hy$index[hy$index == "psri"] <- c("PSRI")

hy$treat[hy$treat == "control"] <- c("Controle")
hy$treat[hy$treat == "b3yr"] <- c("B3yr")
hy$treat[hy$treat == "b1yr"] <- c("B1yr")

hy = hy %>% filter(index %in% c("PSRI","VIG","VARI","MSI"))
  
#Edge - Core separation
diffy = min(hy$y) - max(hy$y)
hy$dist = ((max(hy$y) - hy$y)/diffy)*1000
hy$dist = abs(hy$dist)
summary(hy$dist)

hy$dist2 = c("a")
hy$dist2[hy$dist <= 250] = c("Edge")
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

litter.sub2$Indice = c("Litterfall")
litter.sub2$Ano = as.numeric(litter.sub2$Ano)
litter = litter.sub2 %>% 
  select("Indice","Ano","Tratamento","Valor","Dist")

bms$Indice = c("Biomass")
bms = bms[,c(5,3,1,4,2)]

colnames(land) = c("Variable","Year","Treatment","Value","Dist")
colnames(hy) = c("Variable","Year","Treatment","Value","Dist")
colnames(lai) = c("Variable","Year","Treatment","Value","Dist")
colnames(litter) = c("Variable","Year","Treatment","Value","Dist")
colnames(bms) = c("Variable","Year","Treatment","Value","Dist")

field = rbind(lai, litter, bms)

field2 = field %>% 
  group_by(Variable, Year, Treatment, Dist) %>% 
  summarise(Value = mean(Value)) %>% 
  spread(Variable, Value) %>% 
  unite("id",  Year, Treatment, Dist)


#setwd("C:/Users/Workshop/Documents/Research/Doutorado/Capitulo1/Ecological_Applications/Dados_compartilhar")
#write.csv(land, "land.csv", row.names = F)
#write.csv(hy, "hy.csv", row.names = F)
#write.csv(lai, "lai.csv", row.names = F)
#write.csv(litter, "litter.csv", row.names = F)
#write.csv(bms, "bms.csv", row.names = F)

#Correlation with Landsat Indices ==============================================
land2 = land %>% 
  group_by(Variable, Year, Treatment, Dist) %>% 
  summarise(Value = mean(Value)) %>%
  unite("id",  Year, Treatment, Dist)


dfland = full_join(field2, land2, by = "id")

colnames(dfland)[5] = c("Indice")
dfland = dfland %>% 
  separate(id,  c("Year", "Treatment", "Dist"))

bmsland = dfland[, c(-5,-6)]
bmsland = na.omit(bmsland)

b1 = bmsland %>% 
  filter(Year < 2012) %>% 
  ggplot(aes(Value, Biomass, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Biomass (Mg ha-¹year-¹)", title = "Fire period")+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); b1


b2 = bmsland %>% 
  filter(Year > 2011) %>% 
  ggplot(aes(Value, Biomass, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Biomass (Mg ha-¹year-¹)", title = "Recovery")+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); b2



lailand = dfland[, c(-4,-6)]
lailand = na.omit(lailand)

l1 = lailand %>% 
  filter(Year < 2012) %>% 
  ggplot(aes(Value, LAI, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "LAI (m-² m-²)", title = NULL)+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); l1

l2 = lailand %>% 
  filter(Year > 2011) %>% 
  ggplot(aes(Value, LAI, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "LAI (m-² m-²)", title = NULL)+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); l2


litland = dfland[, c(-4,-5)]
litland = na.omit(litland)

lit1 = litland %>% 
  filter(Year < 2012) %>% 
  ggplot(aes(Value, Litterfall, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Litterfall (Mg ha-¹year-¹)", title = NULL)+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); lit1

lit2 = litland %>% 
  filter(Year > 2011) %>% 
  ggplot(aes(Value, Litterfall, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Litterfall (Mg ha-¹year-¹)", title = NULL)+
  scale_color_manual(values = c("red","blue"))+
  theme_light(); lit2

ggland1 = ggarrange(b1, l1, lit1, ncol = 1, common.legend = T, legend = "bottom")+
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = +3))

ggland2 = ggarrange(b2, l2, lit2, ncol = 1, common.legend = T, legend = "bottom")+
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = +3))

#ggsave(filename = "Fire_Field_Landsat.svg", plot = ggland1,
#       path = "C:/Users/workshop/Documents/Research/Doutorado/Capitulo1/Figuras paper/Suplementary Material",
#       width = 30, height = 20, units =  "cm", dpi = 300)

#ggsave(filename = "recovery_Field_Landsat.svg", plot = ggland2,
#       path = "C:/Users/workshop/Documents/Research/Doutorado/Capitulo1/Figuras paper/Suplementary Material",
#       width = 30, height = 20, units =  "cm", dpi = 300)


#Correlation with Hyperion Indices =============================================
hy2 = hy %>% 
  group_by(Variable, Year, Treatment, Dist) %>% 
  summarise(Value = mean(Value)) %>%
  unite("id",  Year, Treatment, Dist)


dfhy = full_join(field2, hy2, by = "id")

colnames(dfhy)[5] = c("Indice")
dfhy = dfhy %>% 
  separate(id,  c("Year", "Treatment", "Dist"))


bmshy = dfhy[, c(-5,-6)]
bmshy = na.omit(bmshy)

b = ggplot(bmshy, aes(Value, Biomass, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
   #        show.legend = F, r.digits = 1, p.digits = 2)+
  stat_cor(show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Biomass (Mg ha-¹year-¹)")+
  scale_color_manual(values = c("red","blue"))+
  theme_light()


laihy = dfhy[, c(-4,-6)]
laihy = na.omit(laihy)

l = ggplot(laihy, aes(Value, LAI, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  #        show.legend = F, r.digits = 1, p.digits = 2)+
  stat_cor(show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "LAI (m-² m-²)")+
  scale_color_manual(values = c("red","blue"))+
  theme_light()


lithy = dfhy[, c(-4,-5)]
lithy = na.omit(lithy)

lit = ggplot(lithy, aes(Value, Litterfall, col = Dist))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "lm")+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
  #        show.legend = F, r.digits = 1, p.digits = 2)+
  stat_cor(show.legend = F, r.digits = 1, p.digits = 2)+
  facet_wrap(~Indice, scales = "free", nrow = 1)+
  labs(x = NULL, y = "Litterfall (Mg ha-¹year-¹)", title = NULL)+
  scale_color_manual(values = c("red","blue"))+
  theme_light()

gghy = ggarrange(b, l, lit, ncol = 1, common.legend = T, legend = "bottom")+
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = +3))

#ggsave(filename = "Fire_Field_hyperion_C.png", plot = gghy,
#       path = "C:/Users/workshop/Documents/Research/Doutorado/Capitulo1/Figuras paper/Suplementary Material",
#       width = 30, height = 20, units =  "cm", dpi = 300)

#ggsave(filename = "Fire_Field_hyperion_B.svg", plot = gghy,
#       path = "C:/Users/workshop/Documents/Research/Doutorado/Capitulo1/Figuras paper/Suplementary Material",
#       width = 30, height = 20, units =  "cm", dpi = 300)



#Verifying R^2 =================================================================

#Landsat -----------------------------------------------------------------------
head(bmsland)

#Fire Period
bmslf = bmsland %>% 
  group_by(Dist,Indice)%>%
  filter(Year < 2012) %>%
  summarise(
    r2=summary(lm(Biomass~Value))$r.squared,
    pvalue=round(summary(lm(Biomass~Value))$coefficients[2,4],4))

bmslf

lailf = lailand %>% 
  group_by(Dist,Indice)%>%
  filter(Year < 2012) %>%
  summarise(
    r2=summary(lm(LAI~Value))$r.squared,
    pvalue=round(summary(lm(LAI~Value))$coefficients[2,4],4))

lailf

litlf = litland %>% 
  group_by(Dist,Indice)%>%
  filter(Year < 2012) %>%
  summarise(
    r2=summary(lm(Litterfall~Value))$r.squared,
    pvalue=round(summary(lm(Litterfall~Value))$coefficients[2,4],4))

litlf


#Recovery Period
bmslr = bmsland %>% 
  group_by(Dist,Indice)%>%
  filter(Year > 2011) %>% 
  summarise(
    r2=summary(lm(Biomass~Value))$r.squared,
    pvalue=round(summary(lm(Biomass~Value))$coefficients[2,4],4))

bmslr

lailr = lailand %>% 
  group_by(Dist,Indice)%>%
  filter(Year > 2011) %>% 
  summarise(
    r2=summary(lm(LAI~Value))$r.squared,
    pvalue=round(summary(lm(LAI~Value))$coefficients[2,4],4))

lailr

litlr = litland %>% 
  group_by(Dist,Indice)%>%
  filter(Year > 2011) %>% 
  summarise(
    r2=summary(lm(Litterfall~Value))$r.squared,
    pvalue=round(summary(lm(Litterfall~Value))$coefficients[2,4],4))

litlr


#Hyperion ----------------------------------------------------------------------
head(bmshy)

bmshy2 = bmshy %>% 
  group_by(Dist,Indice)%>%
  summarise(
    r2=summary(lm(Biomass~Value))$r.squared,
    pvalue=round(summary(lm(Biomass~Value))$coefficients[2,4],4))


laihy2 = laihy %>% 
  group_by(Dist,Indice)%>%
  summarise(
    r2=summary(lm(LAI~Value))$r.squared,
    pvalue=round(summary(lm(LAI~Value))$coefficients[2,4],4))


lithy2 = lithy %>% 
  group_by(Dist,Indice)%>%
  summarise(
    r2=summary(lm(Litterfall~Value))$r.squared,
    pvalue=round(summary(lm(Litterfall~Value))$coefficients[2,4],4))

bmshy2
laihy2
lithy2
















