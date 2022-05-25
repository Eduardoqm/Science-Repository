#====================================================#
#PCA Hyperspectral Indices by treatmant and Edge-Core#
#                                                    #
#Eduardo Q Marques 08-09-2021                        #
#====================================================#

library(stats)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(extrafont)
library(ggplot2)
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
colnames(df3)[1:3] = c("Ano", "Treatment", "y")

#Change names
df3$Treatment = as.character(df3$Treatment)
df3$Treatment[df3$Treatment == "b1yr"] <- "B1yr"
df3$Treatment[df3$Treatment == "b3yr"] <- "B3yr"
df3$Treatment[df3$Treatment == "control"] <- "Control"

#Plot cluster experiment moment
df3$cond[df3$Ano == 2004] <- "Pre-Fire"
df3$cond[df3$Ano == 2005] <- "Fire"
df3$cond[df3$Ano == 2006] <- "Fire"
df3$cond[df3$Ano == 2008] <- "Fire"
df3$cond[df3$Ano == 2010] <- "Fire"
df3$cond[df3$Ano == 2011] <- "Fire"
df3$cond[df3$Ano == 2012] <- "Post-Fire"

#Edge - Core separation ====================================================================
ggplot(df3, aes(x=EVI, y=y, col=Treatment))+
  geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))

summary(df3$y)
min(df3$y) #-13.08359 = 1000 meters
max(df3$y) #-13.07419 = 0 meters

diffy = min(df3$y) - max(df3$y)
df3$dist = ((max(df3$y) - df3$y)/diffy)*1000
df3$dist = abs(df3$dist)

summary(df3$dist)
ggplot(df3, aes(x=EVI, y=dist, col=Treatment))+
  geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
  scale_color_manual(values = c("orange","red","blue"))


df3$dist2 = c("a")
df3$dist2[df3$dist <= 250] = c("Edge")
df3$dist2[df3$dist > 250] = c("Interior")

#df3$dist[df3$Treatment == "Control"] <- df3$dist - 29.41

#summary(df3$dist)
#ggplot(df3, aes(x=EVI, y=dist, col=cond))+
# geom_point(aes(shape=cond), alpha = 0.35, size = 3)+
#scale_color_manual(values = c("orange","red","blue"))

#PCA Analysis ==============================================================================
#Tutorial video
#Select numeric data
df3 = na.omit(df3)
df4 = df3[,c(-1, -2, -3, -21, -22, -23)]

#Running PCA
df4_pca = PCA(df4, graph = T)
fviz_pca_var(df4_pca, col.var = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07", "#FF0000"), 
             repel = TRUE) # Avoid text overlapping

#Extract variance values
get_eigenvalue(df4_pca)

eigvect = as.data.frame(df4_pca[["var"]][["cor"]])
corrplot::corrplot(df4_pca[["var"]][["cor"]])
#write.table(eigvect, "PCAeig_all_years.csv", sep = ",", row.names = T)

eig_gg = eigvect %>% 
  mutate(name = fct_reorder(rownames(eigvect), desc(Dim.1))) %>% 
  ggplot(aes(x = name, y = Dim.1))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab(NULL)+ylab("Loadings (Autovectores)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text = element_text(family = "Times New Roman", size = 14))

#ggsave(filename = "Eigenvectors_full_hyper.png", plot = eig_gg,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 10, units = "cm", dpi = 300)


#Plot PCAs by momentum ---------------------------------------------------------------------
ggpca = function(w,z){
  w_gg = as.data.frame(w$ind$coord)
  w_gg$Treatment = z$Treatment
  w_gg$Dist = z$dist2
  
  coord = as.data.frame(w$var$coord)
  coord$Dim.1 = coord$Dim.1*10
  coord$Dim.2 = coord$Dim.2*10
  
  ggplot()+
    geom_point(data=w_gg, aes(x=Dim.1, y=Dim.2, shape = Dist, col = Treatment),
               alpha = 0.3, size = 2)+
    geom_segment(data=coord, aes(xend=Dim.1, yend=Dim.2),x=0,y=0,
                 arrow = arrow(length = unit(0.2, "cm")), col = "#525252")+
    geom_text(data=coord, aes(x=Dim.1, y=Dim.2,label=row.names(coord)),vjust=-0.5,size=4)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()+
    xlab(paste0("PC1 ", format(round(w$eig[1,2], 1), nsmall = 1), "%"))+
    ylab(paste0("PC2 ", format(round(w$eig[2,2], 1), nsmall = 1), "%"))+
    scale_color_manual(values=c("orange", "red", "blue"))+
    scale_shape_manual(values = c(17, 19))+
    theme(text = element_text(family = "Times New Roman", size = 14))
}


#Treatment
all_plot =  (ggpca(df4_pca, df3))+ylim(-5, 8.6)+xlim(-22,11)
all_plot

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

pre_plot = (ggpca(pre_pca, pre))+ylim(-3.5, 8)+xlim(-36,11)
pre_plot

#Plot During Fire
fire_pca = PCA(fire[,c(-1, -2, -3, -21, -22, -23)], graph = F)

eigfire = as.data.frame(fire_pca[["var"]][["cor"]])
corrplot::corrplot(fire_pca[["var"]][["cor"]])
#write.table(eigfire, "PCAeig_during_fire.csv", sep = ",", row.names = T)

fire_plot = (ggpca(fire_pca, fire))+ylim(-5.5, 10)+xlim(-21,11)
fire_plot

#Plot Post Fire
post_pca = PCA(post[,c(-1, -2, -3, -21, -22, -23)], graph = F)

eigpost = as.data.frame(post_pca[["var"]][["cor"]])
corrplot::corrplot(post_pca[["var"]][["cor"]])
#write.table(eigpost, "PCAeig_post_fire.csv", sep = ",", row.names = T)

post_plot = (ggpca(post_pca, post))+ylim(-3, 10)+xlim(-18,11)
post_plot

#During Fire in two plots
df3$cond[df3$Ano == 2005] <- "Fire1"
df3$cond[df3$Ano == 2006] <- "Fire1"
df3$cond[df3$Ano == 2008] <- "Fire2"
df3$cond[df3$Ano == 2010] <- "Fire2"
df3$cond[df3$Ano == 2011] <- "Fire2"
fire1 = getcond("Fire1")
fire2 = getcond("Fire2")

#Plot During Fire1 (2005 and 2006)
fire1_pca = PCA(fire1[,c(-1, -2, -3, -21, -22, -23)], graph = F)
fire1_plot = ggpca(fire1_pca, fire1)
fire1_plot

#Plot During Fire2 (2008, 2010 and 2011)
fire2_pca = PCA(fire2[,c(-1, -2, -3, -21, -22, -23)], graph = F)
fire2_plot = ggpca(fire2_pca, fire2)+ylim (-7.3, 7)
fire2_plot

#Save plots ==============================================================
#ggsave(filename = "Hyperspectral Indices (Treatment).png", plot = all_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Pre Fire.png", plot = pre_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral During Fire.png", plot = fire_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Post Fire.png", plot = post_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Fire (2005-2006).png", plot = fire1_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Fire (2008-2010-2011).png", plot = fire2_plot,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 15, height = 15, units = "cm", dpi = 300)


#All Results Panel ======================================================
library(ggpubr)
a = all_plot + ggtitle("A")#+ylim(-5.5, 10)+xlim(-22,11)
b = pre_plot+ ggtitle("B")#+ylim(-3.5, 8)+xlim(-36,11)
c = fire1_plot + ggtitle("C")#+ylim(-5.5, 10)+xlim(-21,11)
d = fire2_plot + ggtitle("D")#+ylim(-3, 10)+xlim(-18,11)


pca_panel = ggarrange(a,b,c,d,
                      common.legend = TRUE,
                      legend="right",
                      ncol = 2, nrow = 2); pca_panel

ggsave(filename = "PCA_panel3.png", plot = pca_panel,
     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion", width = 35, height = 35, units = "cm", dpi = 300)


#Without legend ===============================================================
a = all_plot + ggtitle("A")+theme(legend.position=c(0.2, 0.8))
b = pre_plot+ ggtitle("B")+theme(legend.position="none")
c = post_plot + ggtitle("C")+theme(legend.position="none")

pca_panel2 = ggarrange(a,b, ncol = 1); pca_panel2





#ggsave(filename = "PCA_panel2.png", plot = pca_panel2,
 #      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion",
 #      width = 20, height = 25, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Indices (Treatment).png", plot = a,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion/Panel_ABC", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Pre Fire.png", plot = b,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion/Panel_ABC", width = 15, height = 15, units = "cm", dpi = 300)

#ggsave(filename = "Hyperspectral Post Fire.png", plot = c,
#     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/PCA  Hyperion/Panel_ABC", width = 15, height = 15, units = "cm", dpi = 300)

