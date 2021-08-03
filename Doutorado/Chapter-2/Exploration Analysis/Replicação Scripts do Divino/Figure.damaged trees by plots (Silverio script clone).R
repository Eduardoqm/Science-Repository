#Process to using Silverio's Script: Figure.damaged trees by plots

#Eduardo Q Marques 02-08-2021

#First Part =========================================================================================
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

#Filter Blowdown trees only and data of interest ---------------------------------------------------
df3 = df[,c(1,13,16,14,15,17,3,4,5,6,7,8,9,10,11,12,19,20,22,23,24,25,26,27,30,72,127,128,129)]

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


#Second Part =======================================================================================
#Silverio's Script CLONE ---------------------------------------------------------------------------
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









#Silverio's Script ---------------------------------------------------------------------------------
library(ggplot2)
library(grid)
# figure 3 porcentagem de individuos atingidos pelo vento em cada parcela e em cada classe 
# parte para transformacao dos pontos em utm

sf <- function(x, ...){
  c(s=sum(x, ...), sd=sd(x, ...), l=length(x))
}
####
#setwd("D:\\Dropbox\\IPAM\\projetos\\Savanizacao\\Censo_mortalidade\\area1\\queb.por.vento")
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Relatórios Exploratórios/Comparar dados com Silverio 2019/Scripts Silverio")


d=read.csv("2censo.vento.100.csv",h=T)# mudar nome e salvar novo master
head(d);dim(d)

d$tr=ifelse(d$mm=="t",1,0)
d$met.c <- cut(d$y, c(0,200, 800), labels=c("Edge", "Forest"))
d$dap.c <- cut(d$dap, c(0,20, 120), labels=c("small", "large"))
dim(d[d$pr==1,])
dd=subset(d,is.na(y)==F&is.na(x)==F);dim(dd)
dd2<-dd[!duplicated(dd[,c('x','y')]),];dim(dd2)

dd2$mm=ifelse(dd2$mm=="t-c","t",as.character(dd2$mm))
dd2$cont=1
head(dd2)
doBy::summaryBy(cont~par,dd2,FUN=sum,na.rm=T)

head(d)
d$mm=ifelse(d$mm=="t-c","t",as.character(d$mm))
d$tran=ifelse(d$tran=="AA","A",as.character(d$tran))
d$tran=ifelse(d$tran=="B","AB",as.character(d$tran))
d$cont=1
su=doBy::summaryBy(cont~mm+par+tran,d,FUN=sum,na.rm=T)
su
su2=reshape2::dcast(su[su$par!="",],par+tran~mm, value.var = "cont.sum");su2
su2$tot=rowSums(su2[,3:6],na.rm=T)

su2[is.na(su2)]=0
su2$Crowndamage=su2$c/su2$tot
su2$Snapped=su2$t/su2$tot
su2$Uprooted=su2$r/su2$tot
su2$dam=(su2$c+su2$r+su2$t)/su2$tot
doBy::summaryBy(Crowndamage+Uprooted+Snapped~par,su2,FUN=sd)


###
### calcula intervalo de confiança de mortalidade para as tress parcelas
##3
su=doBy::summaryBy(cont~mm+par+tran+censo,d,FUN=sum,na.rm=T)
su
su2=reshape2::dcast(su[su$par!="",],censo+par+tran~mm, value.var = "cont.sum");su2
su2$tot=rowSums(su2[,4:7],na.rm=T)
su2$dam=rowSums(su2[,4:6],na.rm=T)/su2$tot

head(su2)
su_mort=doBy::summaryBy(dam~par,su2,FUN=sf,na.rm=T)
su_mort$ci=((su_mort$dam.sd/sqrt(su_mort$dam.l))*100)
su_mort



####
#setwd("D:\\Dropbox\\Vento Divino\\Figures\\summary_data")
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Relatórios Exploratórios/Comparar dados com Silverio 2019/Scripts Silverio")
sum3=read.csv("resumo_vento.csv")
head(sum3)
sum3$par=factor(sum3$par3,labels=c("B1yr", "B3yr", "Control"))
sum3$tipod2=factor(sum3$mm2,labels=c("Canopy","Uprooted","Snapped"))


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right


fig3=ggplot(sum3, aes(tipod2, fr,fill=par,color=par)) + 
  geom_bar(position = "dodge", width = 0.5, stat = "identity",alpha=0.7)+
  geom_errorbar(aes(ymin = fr-(fr_se*100), ymax = fr+(fr_se*100)),
                width=.2,
                position=position_dodge(.5))+
  scale_color_manual(values = c("darkorange","brown", "darkgreen"))+
  scale_fill_manual(values = c("orange","brown", "darkgreen"))+
  theme_bw(base_size = 16)+
  labs(x = "", y = "Trees >10 cm DBH (%)")+
  theme(legend.title=element_blank(),
        axis.title.x=element_text(size=16),
        legend.position=c(.25,.75),
        legend.key.width = unit(2, "cm"))

fig3

##########################################
# Salvar o grafico em TIFF maior resolução

#ggsave(filename = "Figure.damaged trees by plots.svg", plot = fig3,
#      path = "D:\\Dropbox\\Vento Divino\\Figures\\Paper figures\\figure 2 - damage frequency and genera", 
#     width = 10, height = 11, units =  "cm", dpi = 300)
#ggsave(filename = "Figure.damaged trees by plots.tiff", plot = fig3,
#      path = "D:\\Dropbox\\Vento Divino\\Figures\\Paper figures\\figure 2 - damage frequency and genera", 
#       width = 9, height = 8, units =  "cm", dpi = 300)










