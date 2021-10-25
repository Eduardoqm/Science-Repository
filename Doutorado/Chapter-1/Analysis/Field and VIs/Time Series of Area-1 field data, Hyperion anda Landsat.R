####################################
# Time Series of Area-1 field data #
# (Hyper and Lansat)               #
# Eduardo Q Marques 18-10-2021     #
####################################

library(tidyverse)
library(reshape2)
library(ggplot2)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data -----------------------------------------------------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

lai = read.csv( "LAI_full_tang.csv", sep = ",")
litt = read.csv( "Liteira_full_tang.csv", sep = ",")
bmas = read.csv( "Biomass_full_tang.csv", sep = ",")

#Modify some elements -------------------------------------------------------------------
#LAI
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

#Litterfall
litt$parcela = as.character(litt$parcela)
litt$dist = as.character(litt$dist)
litt$parcela[litt$parcela == "control"] <- c("Controle")
litt$parcela[litt$parcela == "b3yr"] <- c("B3yr")
litt$parcela[litt$parcela == "b1yr"] <- c("B1yr")
litt$dist[litt$dist == "borda"] <- c("Borda")
litt$dist[litt$dist == "nucleo"] <- c("Interior")
litt = na.omit(litt)
litt$year = as.character(litt$year)
colnames(litt)[1] = c("Tratamento")

#Biomass
bmas$plot = as.character(bmas$plot)
bmas$plot[bmas$plot == "controle"] <- c("Controle")
bmas$plot[bmas$plot == "b3yr"] <- c("B3yr")
bmas$plot[bmas$plot == "b1yr"] <- c("B1yr")
colnames(bmas) = c("Tratamento", "dist", "DBH", "Ano", "biomass")
#bmas$Ano = as.character(bmas$Ano)

#bmas = bmas %>%
  #group_by(Tratamento, Ano) %>% 
  #summarise(biomass = sum(biomass))

#Vegetation Indices =======================================================================
hy = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')
land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#Modify and Filter Landsat data ===========================================================
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

land %>%
  ggplot(aes(x=value, y=dist, col=treat))+
  geom_point(aes(shape = dist2), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))
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

hy %>%
  ggplot(aes(x=value, y=dist, col=treat))+
  geom_point(aes(shape = dist2), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))
colnames(hy) = c("id1","id2","x","y","layer","Indice","Ano","Valor","Tratamento","Dist","Dist2")

#Plot boxplots ---------------------------------------------------------------------------
eqm = c("orange", "red", "blue") #My color palette

#Landsat
ggplot(land, aes(x=Ano, y=Valor, col = Tratamento))+
  geom_jitter(alpha = 0.1)+
  stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Hyperion
ggplot(hy, aes(x=Ano, y=Valor, col = Tratamento))+
  geom_jitter(alpha = 0.1)+
  stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman", size = 14))

#LAI
ggplot(lai, aes(x=Ano, y=LAI, col = Tratamento))+
  geom_jitter(alpha = 0.1)+
  stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  scale_color_manual(values = eqm)+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Litterfall
ggplot(litt, aes(x=year, y=lit_ton_hec, color = Tratamento))+
  geom_jitter(aes(shape = dist, color = Tratamento), size = 1.5, alpha = 0.1)+
  stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  scale_color_manual(values = eqm)+
  theme_bw()+
  labs(y = "Liteira Ton/hec", x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Join VIs and field data =================================================================
land = land %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")
land = land[,c(-1,-2)]

hy = hy %>% 
  filter(Valor < 0.3) %>% 
  select("Indice","Ano","Tratamento","Valor","Dist2")

lai$Indice = c("LAI")
lai$Ano = as.numeric(lai$Ano)
lai = lai %>% 
  select("Indice","Ano","Tratamento","LAI","dist")

litt$Indice = c("Liteira")
litt$year = as.numeric(litt$year)
litt = litt %>% 
  select("Indice","year","Tratamento","lit_ton_hec","dist") %>% 
  filter(year < 2020)

bmas$Indice = c("Biomassa")
bmas = bmas %>% 
  select("Indice","Ano","Tratamento","biomass","dist")

colnames(land) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(hy) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(lai) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(litt) = c("Variavel","Ano","Tratamento","Valor","Dist")
colnames(bmas) = c("Variavel","Ano","Tratamento","Valor","Dist")


df = full_join(land, hy)
df = full_join(df, lai)
df = full_join(df, litt)
#df = full_join(df, bmas)


#Plot data
ggplot(df, aes(x=Ano, y=Valor, color = Tratamento))+
  #geom_boxplot(alpha = 0.7)+
  #geom_jitter(aes(shape = Dist), alpha = 0.1)+
  stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  #stat_summary(geom = "errorbar", fun.data = "mean_cl_boot")+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "black", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "black", size = 1)+
  facet_grid(rows = "Variavel", scales = "free")+
  scale_color_manual(values = eqm)+
  theme_bw()+
  labs(y = NULL)+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Boxplot
df$Ano2 = as.character(df$Ano)
df$Variavel[df$Variavel == "GRND"] <- c("VI - GRND")
df$Variavel[df$Variavel == "PSRI"] <- c("VI - PSRI")


bbx = ggplot(df, aes(x=Ano2, y=Valor,fill = Tratamento))+
  geom_vline(xintercept = "2004",linetype = "dashed", col = "black", size = 1)+
  geom_vline(xintercept = "2011",linetype = "dashed", col = "black", size = 1)+
  geom_jitter(aes(shape = Dist, color = Tratamento), size = 2, alpha = 0.07)+
  geom_boxplot(alpha = 0.7, outlier.alpha = 0)+
  #stat_summary(geom="line", fun.data="mean_cl_boot", size = 1.5, aes(group=Tratamento))+
  #stat_summary(geom="point", fun.data="mean_cl_boot", size = 3, aes(group=Tratamento))+
  #stat_summary(geom = "errorbar", fun.data = "mean_cl_boot")+
  facet_grid(rows = "Variavel", scales = "free")+
  scale_color_manual(values = eqm)+
  scale_fill_manual(values = eqm)+
  scale_shape_manual(values = c(17, 19))+
  theme_bw()+
  labs(y = NULL, x = "Ano")+
  theme(text = element_text(family = "Times New Roman", size = 14))
bbx

ggsave(filename = "Boxplot_Field and VIs.png", plot = bbx,
      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 23, height = 15, units =  "cm", dpi = 300)

#Calculatinf difference =================================================================
df_m = df %>% 
  na.omit() %>% 
  group_by(Ano, Tratamento, Variavel) %>% 
  summarise(Valor = mean(Valor)) %>% 
  ungroup()

#Diference in relation of control by sbtraction
df_crt = filter(df_m, Tratamento == "Controle")
df_b3yr = filter(df_m, Tratamento == "B3yr")
df_b1yr = filter(df_m, Tratamento == "B1yr")

df_b3yr$Valor = 100 - ((df_b3yr$Valor*100)/df_crt$Valor)
df_b1yr$Valor = 100 - ((df_b1yr$Valor*100)/df_crt$Valor)
df_diff = rbind(df_b3yr, df_b1yr)
#colnames(df_diff) = c("Ano", "Tratamento", "Indice","Valor")

diff = ggplot(df_diff, aes(x=Ano, y=Valor, color = Variavel))+
  geom_vline(xintercept = 2004,linetype = "dashed", col = "gray", size = 1)+
  geom_vline(xintercept = 2011,linetype = "dashed", col = "gray", size = 1)+
  geom_line(aes(group = Variavel), size = 1.5, alpha = 0.8)+
  geom_point(size = 1.5, alpha = 0.8)+
  facet_grid(rows = vars(Tratamento))+
  labs(y = "Diferença em relação o Controle (%)")+
  theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  scale_color_manual(values = c('#1b9e77','#ff7f00','#377eb8','darkred'))+
  theme(text = element_text(family = "Times New Roman", size = 14))

diff

#ggsave(filename = "Diff_Field and VIs.png", plot = diff,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Dados de campo", width = 20, height = 10, units =  "cm", dpi = 300)



















