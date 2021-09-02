#Process to using Silverio's Script: Figure.generaxdamaged by plots

#Eduardo Q Marques 03-08-2021

#First Part ========================================================================================
#Join 2021 updated inventory with the blowdown data - EQM 06-07-2021
library(tidyverse)
library(reshape2)

#Open data -----------------------------------------------------------------------------------------
#Blowdown field data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento")

blowdown = read.csv("storm_field_data.csv", sep = ",")

#Inventory data
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Invetarios florestais")

m5_10 = read.csv("master510_area1_Novembro_2020.csv", sep = ",")
m10_20 = read.csv("master1020_area1_Novembro_2020.csv", sep = ",")
m40 = read.csv("master40_area1_Novembro_2020.csv", sep = ",")

#Sucessional group data
suce = read.csv("grupo-sucessional-tanguro.csv", sep = ",")
colnames(suce)[1] = c("codigo")

#Join inventory data -------------------------------------------------------------------------------
m5_10$placa = as.factor(m5_10$placa)
m40$placa = as.factor(m40$placa)
m40$inv = as.character(m40$inv)

#Full join
master = full_join(m5_10, m10_20)
master$obs_cic9 = as.factor(master$obs_cic9)

master = full_join(master, m40)

#Join Blowdown and inventory data ------------------------------------------------------------------
blowdown$placa = as.character(blowdown$placa)
df = full_join(blowdown, master, by = "placa")

df2 = df %>% 
  filter(is.na(data_morta))

#Filter Blowdown trees only and data of interest ---------------------------------------------------
df3 = df2[,c(1,13,16,14,15,17,3,4,5,6,7,8,9,10,11,12,19,20,22,23,24,25,26,27,30,72,127,128,129)]

#Join with Sucessional data ------------------------------------------------------------------------
df4 = full_join(df3, suce)
df5 = df4 %>% 
  filter(placa != "NA")

#Inform some species that no match with the list
df5$succ[df5$codigo == "Tacvul"] <- c("PIO")
df5$succ[df5$codigo == "Inghet"] <- c("CLI")

#Inform some species that no match with the species names
df5$species = as.character(df5$species)
df5$codigo = as.character(df5$codigo)
df5$species[df5$codigo == "Inghet"] <- c("Inga_heterophylla")
df5$species[df5$codigo == "Tacvul"] <- c("Tachigali_vulgaris")

#Calc percentege
df2 = df %>% 
  #na.omit() %>% 
  group_by(genero, tipo_de_dano) %>% 
  summarise(cont = sum(cont))

df2$prc = (df2$cont*100/sum(df2$cont))

maxtree = df %>%  
  group_by(genero) %>% 
  summarise(maxcont = sum(cont))

mdens = df %>% 
  na.omit() %>% 
  group_by(genero) %>% 
  summarise(mean_dens = mean(densidade))

df3 = full_join(df2, maxtree, by = "genero")
df3 = full_join(df3, mdens, by = "genero")
df4 = df3 %>%
  na.omit() %>% 
  filter(maxcont >= 8)


library(ggplot2)
library(ggthemes)
library(doBy)

ggplot(df4, aes(x = reorder(genero, -maxcont), y = prc)) +
  coord_flip()+
  geom_bar(aes(fill = tipo_de_dano),stat = 'identity') +
  ylim(c(-0.02, 10.5))+
  xlab("") +
  ylab("Damage frequency") +
    geom_text(aes(x = reorder(genero, -maxcont), y = -0.02,
                label = maxcont),size=3)+
  #geom_text(aes(x = reorder(genero, -maxcont), y = maxcont + 0.013,
  #             label = mean_dens),size=3)+
  theme_grey(base_size = 13)+
  theme_bw()+
  scale_fill_colorblind()+
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=14),
        axis.text.y = element_text(face = "italic"),
        legend.position=c(.76,.66),
        legend.key.width = unit(1.2, "cm"))



ggplot(df4, aes(x = reorder(genero, -damager), y = dr)) +
  coord_flip()+
  geom_bar(aes(fill = class),stat = 'identity') +
  ylim(c(-0.02,0.27))+
  xlab("") +
  ylab("Damage frequency") +
  geom_text(aes(x = reorder(genero, -damager), y = damager + 0.013,
                label = Wood_density),size=3)+
  geom_text(aes(x = reorder(genero, -damager), y = -0.02,#damager + 0.005,
                label = tot),size=3)+
  theme_grey(base_size = 13)+
  theme_bw()+
  scale_fill_colorblind()+
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=14),
        axis.text.y = element_text(face = "italic"),
        legend.position=c(.76,.66),
        legend.key.width = unit(1.2, "cm"))



















library(ggplot2)
library(grid)

df6 = df5[,c(1,19,18,14)]
df6 = df6 %>% filter(inv %in% c("10a20cm", "40"))
df6$cont = 1

df6$parcela  =as.character(df6$parcela)
df6$tipo_de_dano = as.character(df6$tipo_de_dano)
df6$tipo_de_dano <- replace(df6$tipo_de_dano,is.na(df6$tipo_de_dano), "Intact")

#Calc percentege
prc = df6 %>% 
  #na.omit() %>% 
  group_by(parcela, tipo_de_dano) %>% 
  summarise(cont = sum(cont))
prc = prc[c(-1,-10),]
prc$cont[7] = prc$cont[7]+8

total_a = sum(prc[c(1:4),3])
total_b = sum(prc[c(5:8),3])
total_c = sum(prc[c(9:12),3])

prc$percent = 1
prc$percent[1:4] = ((prc$cont[1:4]*100)/total_a)
prc$percent[5:8] = ((prc$cont[5:8]*100)/total_b)
prc$percent[9:12] = ((prc$cont[9:12]*100)/total_c)

colnames(prc) = c("par", "tipod2", "cont","fr")
prc$par[prc$par == "A"] <-c("Control")
prc$par[prc$par == "B"] <-c("B3yr")
prc$par[prc$par == "C"] <-c("B1yr")


#Plotting ------------------------------------------------------------------------------------------
prc2 = prc %>% filter(tipod2 != "Intact")

ggplot(prc2, aes(tipod2, fr,fill=par,color=par)) + 
  geom_bar(position = "dodge", width = 0.5, stat = "identity",alpha=0.7)+
  #geom_errorbar(aes(ymin = fr-(fr_se*100), ymax = fr+(fr_se*100)),
  #             width=.2,
  #            position=position_dodge(.5))+
  scale_color_manual(values = c("darkorange","brown", "darkgreen"))+
  scale_fill_manual(values = c("orange","brown", "darkgreen"))+
  theme_bw(base_size = 16)+
  labs(x = "", y = "Trees >10 cm DBH (%)")+
  theme(legend.title=element_blank())
#theme(legend.title=element_blank(),
#     axis.title.x=element_text(size=16),
#    legend.position=c(.75,.75),
#   legend.key.width = unit(2, "cm"))




library(ggplot2)
library(ggthemes)
library(doBy)
sumfun <- function(x, ...){
  c(m=mean(x, ...), l=length(x))
}
####

#setwd("D:\\Dropbox\\Vento Divino\\Figures\\summary_data")
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Relatórios Exploratórios/Comparar dados com Silverio 2019/Scripts Silverio")
dir()
d=read.csv("22censo.vento.100_2016.csv",h=T)# mudar nome e salvar novo master
d$mm2=ifelse(d$mm=="t-c","t",as.character(d$mm))
d$mm2=factor(d$mm2,labels=c("Crown-damage","Uprooted","Snapped","vi"))
d$contador=1
head(d)

d0=d[d$mm2=="Snapped",]
tot0=as.data.frame(table(d0$genero,d0$contador));length(tot0)
totx=as.data.frame(table(d$genero,d$contador));length(totx)
totx2=merge(tot0,totx,by.x="Var1",by.y="Var1")
tot=subset(totx2,select=c(Var1,Freq.x,Freq.y))
head(tot)
names(tot)=c("genero","snapped","tot")
#d2=d[d$mm2!="vi",]
d2=d[d$mm2!="vi",]
tot2=summaryBy(densidade~genero,d2, FUN=sumfun,na.rm=T);length(tot2)
tot3=merge(tot,tot2,by.x="genero",by.y="genero")
head(tot3)

d2=summaryBy(contador~mm2+genero,d2,FUN=function(x, ...){l=length(x)},na.rm=T)
names(d2)=c("class","genero","fa")
head(d2)

d3=merge(d2,tot3,by.x = "genero", by.y = "genero")
names(d3)=c("genero","class","fa","snapped","tot","Wood_density","damage")     
head(d3)  

d4=subset(d3,tot>50&genero!="unknow")
d4$dr=d4$fa/d4$tot
d4$damager=d4$damage/d4$tot
d4$snappedr=d4$snapped/d4$tot
d4$Wood_density=round(d4$Wood_density,2)
head(d4)

d5=subset(d4,class=="Snapped",select = c(genero,fa,dr))
names(d5)=c("genero","tr.fa","tr.fr")

d6=merge(d4,d5,by.x = "genero",by.y = "genero")
head(d6)
##plot
fs4=ggplot(d4, aes(x = reorder(genero, -damager), y = dr)) +
  coord_flip()+
  geom_bar(aes(fill = class),stat = 'identity') +
  ylim(c(-0.02,0.27))+
  xlab("") +
  ylab("Damage frequency") +
  geom_text(aes(x = reorder(genero, -damager), y = damager + 0.013,
                label = Wood_density),size=3)+
  geom_text(aes(x = reorder(genero, -damager), y = -0.02,#damager + 0.005,
                label = tot),size=3)+
  theme_grey(base_size = 13)+
  theme_bw()+
  scale_fill_colorblind()+
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=14),
        axis.text.y = element_text(face = "italic"),
        legend.position=c(.76,.66),
        legend.key.width = unit(1.2, "cm"))


fs4
##########################################
# Salvar o grafico em TIFF maior resolução

#ggsave(filename = "Figure.generoxdamage.svg", plot = fs4,
#       path = "D:\\Dropbox\\Vento Divino\\Figures\\Paper figures\\figure 2 - damage frequency and genera", 
#       width = 13, height = 14, units =  "cm", dpi = 300)

#ggsave(filename = "Figure4.genero.tiff", plot = fs4,
#       path = "D:\\Dropbox (DadosTanguro)\\Vento Divino\\Figures\\suplemmentary information", 
#       width = 12, height = 16, units =  "cm", dpi = 300)


###

