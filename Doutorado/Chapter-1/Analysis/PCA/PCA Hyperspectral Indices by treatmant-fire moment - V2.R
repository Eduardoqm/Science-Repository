#=======================================#
#PCA Hyperspectral Indices by treatmant #
#                                       #
#Eduardo Q Marques 04-03-2020           #
#=======================================#

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
  select(year, treat)


#Join everything
df3 = cbind(treat$year, treat$treat,evi,ndvi,vari,vig,msi,ndii,ndwi,pssr,psri,sipi,wbi,pri,rendvi,nirv, lwvi2, nbr, nbr2)
colnames(df3)[1:2] = c("Ano", "Parcela")

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

#Removing Outliers =========================================================================
#boxplot(df3$PRI) #Looking for outliers
#outliers <- boxplot(df3$PRI, plot=FALSE)$out #Storing outliers into a vector
#print(outliers) #Check the results
#df3$PRI[which(df3$PRI %in% outliers),] #Find in which rows the outliers are
#df3b <- df3[-which(df3$PRI %in% outliers),] #Remove the rows with the outliers
#df3[which(df3$PRI %in% outliers),] <- NA
#df3$PRI[df3$PRI == outliers] <- NA
#boxplot(df3$PRI)


df3b = df3 #Copy from original
df3b$cont = 1

#Function to find, store and remove rows with outliers
outrem = function(x){
  outliers <- boxplot(x, plot=FALSE)$out #Storing outliers into a vector
  z = df3b[-which(x %in% outliers),] #Remove the rows with the outliers
}

#Looping process
for (w in 3:19) {
  boxplot(df3b[,c(-1,-2,-20,-21)])
  df3b = outrem(df3b[,w])
  print(sum(df3b$cont))
}

#Comparison
boxplot(df3[,c(-1,-2,-20)])
boxplot(df3b[,c(-1,-2,-20,-21)])

df3 = df3b[,-21]
#PCA Analysis ==============================================================================
#Tutorial video
#Select numeric data
df3 = na.omit(df3)
#df4 = df3[,c(-1, -2, -17)]
df4 = df3[,c(-1, -2, -20)]

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

library(ggstar)
ggplot(eigvect, aes(x = rownames(eigvect), y = Dim.1, fill = Dim.1, col = Dim.1))+
  geom_star(starshape=1, size=2.5)+
  geom_hline(yintercept = 0, linetype = "dashed")

#write.table(eigvect, "PCAeig_all_years.csv", sep = ",", row.names = T)

#Plot PCAs by momentum ---------------------------------------------------------------------
#Treatment
grp = as.factor(df3[,c(2)])
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
pre_pca = PCA(pre[,c(-1,-2,-20)], graph = F)

eigpre = as.data.frame(pre_pca[["var"]][["cor"]])
corrplot::corrplot(pre_pca[["var"]][["cor"]])
#write.table(eigpre, "PCAeig_pre_fire.csv", sep = ",", row.names = T)

grp = as.factor(pre[,c(2)])
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
