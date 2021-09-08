#====================================================#
#PCA Hyperspectral Indices by treatmant and Edge-Core#
#                                                    #
#Eduardo Q Marques 04-03-2020                        #
#====================================================#

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)

#Data =============================================================================================
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1")

#df = read.csv("Hyperion_indexs_all_xy.csv", sep = ',')
df = read.csv("Hyperion_indexs_all_xy-B.csv", sep = ',')

#Transform data for analysis ======================================================================
#df2 = df[c(5,7)]
df2 = df %>% 
  select(index, value)
#df2$value = log(df2$value)

#Transpor variables in columns
transp = function(x){
  z = df2 %>% 
    filter(index == x)
  z = as.data.frame(z[,c(2)])
}

ari = transp("ari");colnames(ari) = c("ARI")
evi = transp("evi2");colnames(evi) = c("EVI")
ndvi = transp("ndvi");colnames(ndvi) = c("NDVI")
vari = transp("vari");colnames(vari) = c("VARI")
vig = transp("vig");colnames(vig) = c("VIG")
lwvi2 = transp("lwvi2");colnames(lwvi2) = c("LWVI2")
msi = transp("msi");colnames(msi) = c("MSI")
ndii = transp("ndii");colnames(ndii) = c("NDII")
ndwi = transp("ndwi");colnames(ndwi) = c("NDWI")
pssr = transp("pssr");colnames(pssr) = c("PSSR")
psri = transp("psri");colnames(psri) = c("PSRI")
sipi = transp("sipi");colnames(sipi) = c("SIPI")
wbi = transp("wbi");colnames(wbi) = c("WBI")
pri = transp("pri");colnames(pri) = c("PRI")
rendvi = transp("rendvi");colnames(rendvi) = c("RENDVI")
nirv = transp("nirv"); colnames(nirv) = c("NIRv")
nbr = transp("nbr"); colnames(nbr) = c("NBR")
nbr2 = transp("nbr2"); colnames(nbr2) = c("NBR2")


#Stract treatmant name
#treat = df[c(1:11781),]#; treat = treat[,c(6,7,8)]
treat = df[c(1:12019),]
treat = treat %>% 
  select(year, treat, y)


#Join everything
df3 = cbind(treat$year, treat$treat, treat$y,  evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv, lwvi2, nbr, nbr2)
colnames(df3)[1:3] = c("Ano", "Parcela", "y")

#Change names
df3$Parcela = as.character(df3$Parcela)
df3$Parcela[df3$Parcela == "b1yr"] <- "B1yr"
df3$Parcela[df3$Parcela == "b3yr"] <- "B3yr"
df3$Parcela[df3$Parcela == "control"] <- "Controle"

#Plot cluster experiment moment
df3$cond[df3$Ano == 2004] <- "Pre-Fire"
df3$cond[df3$Ano == 2005] <- "Fire"
df3$cond[df3$Ano == 2006] <- "Fire"
df3$cond[df3$Ano == 2008] <- "Fire"
df3$cond[df3$Ano == 2010] <- "Fire"
df3$cond[df3$Ano == 2011] <- "Fire"
df3$cond[df3$Ano == 2012] <- "Post-Fire"

#Edge - Core separation ====================================================================
ggplot(df3, aes(x=EVI, y=y, col=Parcela))+
  geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))

summary(df3$y)
min(df3$y) #-13.08359 = 1000 meters
max(df3$y) #-13.07419 = 0 meters

diffy = min(df3$y) - max(df3$y)
df3$dist = ((max(df3$y) - df3$y)/diffy)*1000
df3$dist = abs(df3$dist)

summary(df3$dist)
ggplot(df3, aes(x=EVI, y=dist, col=Parcela))+
  geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))


df3$dist2 = c("a")
df3$dist2[df3$dist <= 250] = c("Borda")
df3$dist2[df3$dist > 250] = c("Interior")

#df3$dist[df3$Parcela == "Controle"] <- df3$dist - 29.41

#summary(df3$dist)
#ggplot(df3, aes(x=EVI, y=dist, col=cond))+
 # geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
  #scale_color_manual(values = c("orange","red","blue"))

#PCA Analysis ==============================================================================
#Tutorial video
#Select numeric data
df3 = na.omit(df3)
#df4 = df3[,c(-1, -2, -17)]
df4 = df3[,c(-1, -2, -3, -21, -22, -23)]

#Running PCA
df4_pca = PCA(df4, graph = T)
fviz_pca_var(df4_pca, col.var = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07", "#FF0000"), 
             repel = TRUE # Avoid text overlapping
)

#Extract variance values
get_eigenvalue(df4_pca)

eigvect = as.data.frame(df4_pca[["var"]][["cor"]])
corrplot::corrplot(df4_pca[["var"]][["cor"]])
#write.table(eigvect, "PCAeig_all_years.csv", sep = ",", row.names = T)

#Plot PCAs by momentum ---------------------------------------------------------------------
df4_gg = as.data.frame(df4_pca$ind$coord)
df4_gg$Parcela = df3$Parcela
df4_gg$Dist = df3$dist2

ggplot(df4_gg, aes(x=Dim.1, y=Dim.2, col = Parcela))+
  geom_point(aes(shape = Dist), alpha = 0.3, size = 2)+
  theme_minimal()+
  scale_color_manual(values=c("orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))







#Treatment
grp = as.factor(df3[,c(2)])
grp2 = as.factor(df3[,c(23)])
fviz_pca_biplot(df4_pca, habillage = grp,
                #addEllipses=TRUE,
                #ellipse.level=0.99,
                pointshape = 19,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.3,
                title = NULL, legend.title = "Parcela")+
  #xlim(-20, 20) + ylim (-20, 20)+
  scale_color_manual(values=c("orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))


#Separate data by momentum
getcond = function(x){
  df3 %>% 
    na.omit() %>% 
    filter(cond == x)
}

pre = getcond("Pre-Fire")
fire = getcond("Fire")
post = getcond("Post-Fire")

#Plot Pre Fire
pre_pca = PCA(pre[,c(-1, -2, -3, -21, -22, -23)], graph = F)

eigpre = as.data.frame(pre_pca[["var"]][["cor"]])
corrplot::corrplot(pre_pca[["var"]][["cor"]])
#write.table(eigpre, "PCAeig_pre_fire.csv", sep = ",", row.names = T)

grp = as.factor(pre[,c(2)])
grp2 = as.factor(pre[,c(23)])
fviz_pca_biplot(pre_pca, habillage = grp,
                pointshape = 19, pointsize = 2,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.4,
                title = NULL, legend.title = "Parcela")+
  xlim(-15, 10) + ylim (-3, 5)+
  scale_color_manual(values=c( "orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Plot During Fire
fire_pca = PCA(fire[,c(-1,-2,-20)], graph = F)

eigfire = as.data.frame(fire_pca[["var"]][["cor"]])
corrplot::corrplot(fire_pca[["var"]][["cor"]])
#write.table(eigfire, "PCAeig_during_fire.csv", sep = ",", row.names = T)

grp = as.factor(fire[,c(2)])
fviz_pca_biplot(fire_pca, habillage = grp,
                pointshape = 19, pointsize = 2,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.4,
                title = NULL, legend.title = "Parcela")+
  #ylim (-12.5, 10)+
  scale_color_manual(values=c( "orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Plot Post Fire
post_pca = PCA(post[,c(-1,-2,-20)], graph = F)

eigpost = as.data.frame(post_pca[["var"]][["cor"]])
corrplot::corrplot(post_pca[["var"]][["cor"]])
#write.table(eigpost, "PCAeig_post_fire.csv", sep = ",", row.names = T)

grp = as.factor(post[,c(2)])
fviz_pca_biplot(post_pca, habillage = grp,
                pointshape = 19, pointsize = 2,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.4,
                title = NULL, legend.title = "Parcela")+
  scale_color_manual(values=c( "orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))+
  ylim (-3, 5)


#During Fire in two plots
df3$cond[df3$Ano == 2005] <- "Fire1"
df3$cond[df3$Ano == 2006] <- "Fire1"
df3$cond[df3$Ano == 2008] <- "Fire2"
df3$cond[df3$Ano == 2010] <- "Fire2"
df3$cond[df3$Ano == 2011] <- "Fire2"
fire1 = getcond("Fire1")
fire2 = getcond("Fire2")

#Plot During Fire1 (2005 and 2006)
fire1_pca = PCA(fire1[,c(-1,-2,-20)], graph = F)
grp = as.factor(fire1[,c(2)])
fviz_pca_biplot(fire1_pca, habillage = grp,
                pointshape = 19, pointsize = 2,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.4,
                title = NULL, legend.title = "Parcela")+
  ylim (-7, 5)+
  scale_color_manual(values=c( "orange", "red", "blue"))+
  theme(text = element_text(family = "Times New Roman", size = 14))

#Plot During Fire2 (2008, 2010 and 2011)
fire2_pca = PCA(fire2[,c(-1,-2,-20)], graph = F)
grp = as.factor(fire2[,c(2)])
fviz_pca_biplot(fire2_pca, habillage = grp,
                pointshape = 19, pointsize = 2,
                geom.ind = c("point"),
                col.var = "black", alpha = 0.4,
                title = NULL, legend.title = "Parcela")+
  scale_color_manual(values=c( "orange", "red", "blue"))+
  ylim (-10, 10)+
  theme(text = element_text(family = "Times New Roman", size = 14))
