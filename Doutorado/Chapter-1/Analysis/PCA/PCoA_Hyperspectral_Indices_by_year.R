#===========================================================#
# PCoA Hyperspectral Indices  by Years                      #
# and PCoA Hyperspectral and Multispectral Indices Together #                                #                                                           #
#                                                           #
# Eduardo Q Marques 17-11-2020                              #
#===========================================================#

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

hy = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')

land = read.csv("Landsat_indexs_all_xy.csv", sep = ',')

#For Hyperpectral data ============================================================================
#Transform data for analysis
hy2 = hy[c(5,7)]
#hy2$value = log(hy2$value)

#Transpor variables in columns
transp = function(x){
  z = hy2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

ari = transp("ari");colnames(ari) = c("ari")
evi = transp("evi2");colnames(evi) = c("evi")
ndvi = transp("ndvi");colnames(ndvi) = c("ndvi")
vari = transp("vari");colnames(vari) = c("vari")
vig = transp("vig");colnames(vig) = c("vig")
lwvi2 = transp("lwvi2");colnames(lwvi2) = c("lwvi2")
msi = transp("msi");colnames(msi) = c("msi")
ndii = transp("ndii");colnames(ndii) = c("ndii")
ndwi = transp("ndwi");colnames(ndwi) = c("ndwi")
pssr = transp("pssr");colnames(pssr) = c("pssr")
psri = transp("psri");colnames(psri) = c("psri")
sipi = transp("sipi");colnames(sipi) = c("sipi")
wbi = transp("wbi");colnames(wbi) = c("wbi")
pri = transp("pri");colnames(pri) = c("pri")
rendvi = transp("rendvi");colnames(rendvi) = c("rendvi")
nirv = transp("nirv"); colnames(nirv) = c("nirv")

#Stract treatmant name
treat = hy[c(1:11781),]; treat = treat[,c(6,7,8)]

#Join everything
hy3 = cbind(treat$parcela,treat$year,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv)
colnames(hy3)[1] = "Parcela"; colnames(hy3)[2] = "year"

#Make data frame by year
getyear = function(x){
  hy3 %>% 
    na.omit() %>% 
    filter(year == x)
}

p2004 = getyear(2004)
p2005 = getyear(2005)
p2006 = getyear(2006)
p2008 = getyear(2008)
p2010 = getyear(2010)
p2011 = getyear(2011)
p2012 = getyear(2012)

#PCoA Analysis
#Create a distance data
p2004b = p2004[,c(-1,-2)]
p2004c = as.matrix(p2004b)

library(ape)
library(vegan)
veg = vegdist(p2004c, "bray")
res = pcoa(veg)

res$values
biplot(res)

























































































library(vegan)

## criar uma matriz de dissimilaridade usando Bray-Curtis
especies.bray<-vegdist(p2004c,"bray")

## fazer a PCoA usando a função cmdscale
PCoA<-cmdscale(especies.bray, k=1, eig=T) ## k=número de eixos; eig=T para ver a explicação
PCoA1<-cmdscale(especies.bray, k=2, eig=T) ## k=número de eixos; eig=T para ver a explicação
names(PCoA1)
#biplot(PCoA1)
PCoA.scores=(as.data.frame(cbind(PCoA1$points, PCoA1$eig)))
PCoA.eig=(as.data.frame(cbind(PCoA$eig)))
colnames(PCoA.scores)=c("Eixo1", "Eixo2", "Autovalor")## alterar caso queira salvar mais de dois eixos
PCoA.expl=(as.data.frame(cbind(PCoA$GOF, PCoA1$GOF)))## salvar a explicação dos eixos
colnames(PCoA.expl)=c("Eixo1", "Eixo 1 e 2")## alterar caso queira salvar mais de dois eixos
# edit(PCoA.eig)

## Salvar os resultados
path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo1/PCoA"
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo1/PCoA")
dir()

write.csv(PCoA.scores, "trees.PCoA.scores.stream_field.csv")
write.csv(PCoA.eig, "trees.PCoA.eigenvalue.stream_field.csv")
write.csv(PCoA.expl, "trees.PCoA.explicação.stream_field.csv", na=" ", row.names=F)
summary(PCoA1)


####### Anosim testar significância da separação entre grupos
###MOdelo
#library(vegan)
#data(dune) # matriz primária
#data(dune.env) # matriz secundária
#anosim<-anosim(dune, dune.env$Management, permutations = 999, distance = "bray")
#summary(anosim)
#plot(anosim)

####### Anosim testar significância da separação entre grupos para forma de uso da terra
library(vegan)
anosim<-anosim(p2004c, p2004$Parcela, permutations = 999, distance = "bray")
summary(anosim)
plot(anosim)

rm (list=ls())

### Plotando resultados PCoA

path="C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo1/PCoA"
setwd("C:/Users/Eduardo Q Marques/Documents/My Jobs/Doutorado/Capitulo1/PCoA")
dir()

fi=read.csv("trees.PCoA.scores.stream_field.csv",h=T)
head(fi)
names(fi)

fi$Landuse=factor(fi$X,levels = c("Forest-Stream","Forest-Upland","Cropland-Stream","Cropland-Edge"))


library(ggplot2)

# scale_shape_manual
# http://www.sthda.com/english/wiki/ggplot2-point-shapes
### Ploot for trees
plot=ggplot(data = fi,aes(Eixo1,Eixo2))+
  geom_point(data = fi,aes(shape = Landuse,colour = Landuse),size=5,alpha=0.9)+
  scale_shape_manual(values=c(16,17,15,9))+
  # scale_color_manual(values = c('orange', 'darkgreen'))+
  # scale_fill_manual(values =  c('orange', 'darkgreen'))+
  stat_ellipse(data = fi, aes(colour = Landuse, linetype=Landuse), lwd=1.9)+
  labs(x = "Axis 1 \n(16.6%; eigenvalue = 5.0)", y="Axis 2 \n(10.9%; eigenvalue = 3.3)")+
  # annotate("text", x = c(-0.7), y = c(0.45), label = c("a"),size=10)+
  # annotate("text", x = c(-0.5), y = c(0.25), label = c("Trees"),size=10)+
  # theme_bw(base_size = 16)+
  # scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  # scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))+
  scale_color_manual(values = c("darkgreen","chartreuse1", "orange","saddlebrown"))+
  scale_fill_manual(values = c("darkgreen","chartreuse1", "orange","saddlebrown"))+
  theme_light()+
  # theme_bw(base_size = 16)+
  theme_minimal(base_size = 16)+
  theme(legend.justification = c(0,0),legend.position = c(.1,.1),
        legend.title=element_blank(),
        axis.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 25),
        text = element_text(size = 20));plot































dist2004 = dist(scale(t(p2004b), center = T, scale = T), method = "euclidean")

mds2004 = cmdscale(dist2004, eig = T, x.ret = T)
mds.var.per = round(mds2004$eig/sum(mds2004$eig)*100, 1)

#Make data better to plot
mds_val = mds2004$points
mds_data = data.frame(Sample = rownames(mds_val),
                      X=mds_val[,1],
                      Y=mds_val[,2])

#Plot PCoA
ggplot(mds_data, aes(x=X, y=Y, label=Sample))+
  geom_text()+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep=""))+
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep=""))
