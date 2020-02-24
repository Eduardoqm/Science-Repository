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
#hy = hy[,c(-12)] #PSSR have so high values in comparison with other indexs
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
#df = full_join(df, biomass, by="id")
#df = full_join(df, lai, by="id")
#df = full_join(df, litt, by="id")
#df = full_join(df, fuel, by="id")

df = df %>% 
  separate(col = "id", c("parcela", "data", "dist"), sep = '_')

#Only Control plot
df_crt = df %>% 
  filter(parcela == "controle")


#Difference by start year
df_crt = melt(df_crt)
diff = df_crt %>% 
  filter(data == 2004)

diff = data.frame(diff)
diff = diff[rep(seq_len(nrow(diff)), each = 7),]


df_crt$diff = ((df_crt$value - diff$value)*100)/df_crt$value

#Plot data =========================
#gg_crt = melt(df_crt)
df_crt = df_crt[,c(-5)]
colnames(df_crt) = c("parcela","date","dist", "index", "value")
df_crt = df_crt %>% 
  filter(value != 0)
gg1 = df_crt %>% filter(dist == 'borda')
gg2 = df_crt %>% filter(dist == 'nucleo')


ggplot(gg1, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  #geom_line(aes(group=index), size = 1)+
  #geom_point()+
  #labs(fill= "Plot",x="Year",y="B1yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  annotate("text", x = c(3.5, 5.5), y = 50, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(gg2, aes(date,value, col=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  #geom_line(aes(group=index), size = 1)+
  #geom_point()+
  labs(fill= "Plot",x="Year",y="B3yr - Control (% Relative difference Edge)")+
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")+
  
  #annotate("text", x = c(3.5, 5.5), y = 0, label = "Dry")+
  
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))


#Struture ========
struc_edge = gg1 %>% 
  filter(index %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))

a = ggplot(struc_edge, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Edge")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

struc_core = gg2 %>% 
  filter(index %in% c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))

b = ggplot(struc_core, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Core")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

struc = ggarrange(a + rremove("xlab"), b + rremove("xlab"),
                  common.legend = TRUE,
                  legend="bottom",
                  ncol = 1, nrow = 2)


#Biochemistry ========
bioc_edge = gg1 %>% 
  filter(index %in% c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))

a = ggplot(bioc_edge, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Edge")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

bioc_core = gg2 %>% 
  filter(index %in% c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))

b = ggplot(bioc_core, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Core")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

bioc = ggarrange(a + rremove("xlab"), b + rremove("xlab"),
                 common.legend = TRUE,
                 legend="bottom",
                 ncol = 1, nrow = 2)

#Physiologic ========
phy_edge = gg1 %>% 
  filter(index %in% c('pri','rendvi','biomass','lai','litter','fuel'))

a = ggplot(phy_edge, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Edge")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

phy_core = gg2 %>% 
  filter(index %in% c('pri','rendvi','biomass','lai','litter','fuel'))

b = ggplot(phy_core, aes(date,value, fill=index))+ 
  geom_bar(position = "dodge", stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  labs(fill= "Index",x="Year",y="Core")+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "gray", fill=NA, size=0.5))

phy = ggarrange(a + rremove("xlab"), b + rremove("xlab"),
                common.legend = TRUE,
                legend="bottom",
                ncol = 1, nrow = 2)





