#Correlation Matrix Hyperion and full plot data
#By: Eduardo Q Marques 24-03-2020
#Obs: looking Analysis Cap1.R

library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggpubr)

#Load data ==========================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Deposito/Banco de Dados Tanguro/Dados para analise cap1")


hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
#hy = hy[,c(-5, -9)] #NDWI have so high values in comparison with other indexs
hy$year = as.character(hy$year)

biomass = read.csv("Biomass_full_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_full_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_full_tang.csv", sep = ",", header = TRUE)

fuel = read.csv("Fuel_full_tang.csv", sep = ",", header = TRUE)

#Data integration =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(year <=2012)

#LAI
lai = lai %>% 
  filter(year <=2012)

#Litterfall
litt = litt %>% 
  filter(year <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')
colnames(biomass) = c("id", "biomass", "dist")

lai = lai %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')
colnames(litt) = c("id", "litter", "dist")

fuel = fuel %>% 
  unite(col = "id", c("parcela", "year"), sep = '_')
colnames(fuel) = c("id", "fuel", "dist")

hy2 = hy[,c(4,5,6)]
#hy3 = t(hy2)
hy2 = hy2 %>% 
  na.omit()

hy3 = reshape(hy2, v.names = "value", idvar = "id", timevar="index",
        direction="wide")



df = hy
#df1 = full_join(df, biomass, by="id")
#df2 = full_join(df, lai, by="id")
#df3 = full_join(df, litt, by="id")
df4 = full_join(df, fuel, by="id")

df4 = df4 %>% 
  separate(col = "id", c("parcela", "year"), sep = '_')

#Simple Correlation Matrix =============================================================
#Fuel
fuel_cor = df4[,c(4,5,6,7,9)]
ggcorr(fuel_cor, label = TRUE)

#Correlations GGPLOT =======================================================================
#Making data
hy2 = melt(hy)
df2 = hy2

df2 = full_join(hy2, lai, by="id")
df2 = full_join(df2, litt, by="id")
df2 = full_join(df2, biomass, by="id")
df2 = full_join(df2, fuel, by="id")

colnames(df2) = c('id', 'index', 'value', 'lai', 'litter', 'biomass', 'fuel')

df2 = df2 %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')



eqm = c("#FC4E07","#00AFBB") #Pallete colors(Orange and Blue)


#Structural
struc = df2 %>% 
  filter(index %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))


a = ggplot(struc, aes(x=value, y=lai, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

a = ggpar(a, palette = eqm)



b = ggplot(struc, aes(x=value, y=litter, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

b = ggpar(b, palette = eqm)

struc2 = ggarrange(a + rremove("xlab"),
                   b + rremove("xlab"),
                   common.legend = TRUE,
                   legend="bottom",
                   ncol = 1, nrow = 2)

struc2


#Biochemistry
bioc = df2 %>% 
  filter(index %in% c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))


bioc_a = ggplot(bioc, aes(x=value, y=lai, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F, label.y = 6, colour = "black")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

bioc_lai = ggpar(bioc_a, palette = eqm)



bioc_b = ggplot(bioc, aes(x=value, y=litter, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F, label.y = 0.37, colour = "black")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

bioc_litt = ggpar(bioc_b, palette = eqm)

bioc_lai; bioc_litt


#Physiologic
phy = df2 %>% 
  filter(index %in% c('pri','rendvi','biomass','lai','litter','fuel'))


a = ggplot(phy, aes(x=value, y=lai, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

a = ggpar(a, palette = eqm)



b = ggplot(phy, aes(x=value, y=litter, color=dist))+
  geom_point(size=3)+
  geom_smooth(method="lm", se=F)+ 
  facet_wrap(~index, scales="free") +
  stat_cor(show.legend = F)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))+
  theme(legend.position="bottom")

b = ggpar(b, palette = eqm)

phy2 = ggarrange(a + rremove("xlab"),
                 b + rremove("xlab"),
                 common.legend = TRUE,
                 legend="bottom",
                 ncol = 1, nrow = 2)

phy2