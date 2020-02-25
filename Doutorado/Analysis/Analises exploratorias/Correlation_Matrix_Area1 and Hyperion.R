#Correlation Matrix Cap-1 Hyperion Indexs on Fire
#By: Eduardo Q Marques 20-02-2020
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

hy = read.csv("Hyperion_indexs_median by plot.csv", sep = ",", header = TRUE)
hy = hy[,c(-9)] #NDWI have so high values in comparison with other indexs

biomass = read.csv("Biomass_tang.csv", sep = ",", header = TRUE)

lai = read.csv("LAI_tang.csv", sep = ",", header = TRUE)

litt  = read.csv("Liteira_tang.csv", sep = ",", header = TRUE)

fuel = read.csv("Fuel_tang.csv", sep = ",", header = TRUE)

#fire = read.csv("Fire.csv", sep = ",", header = TRUE) #Make the fire intensity!


#Data integration =======================================================================
#From Hyperion we have 2004:2012, so let's try use other datas in this range
#Biomass
biomass = biomass %>% 
  filter(data <=2012)

#LAI
lai = lai %>% 
  filter(data <=2012)

#Litterfall
litt = litt %>% 
  filter(data <=2012)

#Join Data
hy = hy %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

biomass = as.data.frame(biomass)#To work
biomass = biomass %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(biomass) = c("id", "biomass")

lai = lai %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')

litt = litt %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(litt) = c("id", "litter")

fuel = fuel %>% 
  unite(col = "id", c("parcela", "data", "dist"), sep = '_')
colnames(fuel) = c("id", "fuel")


df = hy
df = full_join(df, biomass, by="id")
df = full_join(df, lai, by="id")
df = full_join(df, litt, by="id")
df = full_join(df, fuel, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')


#Correlations =============================================================================
#Strutural
struc = df %>% 
  select('dist','evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')

ggcorr(struc, label = TRUE)
p<-ggpairs(struc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('evi','ndvi','nbri','vari','vig','lai','litter'))
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#FC4E07"))  
  }
}
p

#Biochemistry
bioc = df %>% 
  select('dist','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggcorr(bioc, label = TRUE)
p<-ggpairs(bioc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','lai','litter'))
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#FC4E07"))  
  }
}
p

#Physiologic
phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')

ggcorr(phy, label = TRUE)
p<-ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
           columns = c('pri','rendvi','lai','litter'))
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#FC4E07"))  
  }
}
p





#Correlations GGPLOT =======================================================================

hy2 = melt(hy)
df2 = hy2

df2 = full_join(hy2, lai, by="id")
df2 = full_join(df2, litt, by="id")
df2 = full_join(df2, biomass, by="id")
df2 = full_join(df2, fuel, by="id")

colnames(df2) = c('id', 'index', 'value', 'lai', 'litter', 'biomass', 'fuel')

df2 = df2 %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')



eqm = c("#00AFBB", "#FC4E07")

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