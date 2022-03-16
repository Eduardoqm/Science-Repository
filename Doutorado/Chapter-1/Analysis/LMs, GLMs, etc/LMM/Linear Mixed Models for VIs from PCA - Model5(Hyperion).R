###########################################################
#Linear Mixed Models for VIs from PCA - Model5 (Hyperion) #
#                                                         #
#Eduardo Q Marques 16-03-2022                             #
###########################################################

library(tidyverse)
library(reshape2)
library(ggeffects)
library(ggpubr)
library(sjPlot)
library(lattice)
library(lme4)
library(factoextra)
library(extrafont)
font_import()
loadfonts(device = "win", quiet = TRUE)


dir_input <- "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1"

list.files(path = dir_input)

rs <- read_csv(file.path(dir_input, "Hyperion_indexs_all_xy-B.csv"))
rs$treat <- ifelse(rs$treat == "control","aControl", as.character(rs$treat))
rs$treat <- ifelse(rs$treat == "b3yr", "B3yr", as.character(rs$treat))
rs$treat <- ifelse(rs$treat == "b1yr", "B1yr", as.character(rs$treat))

#Calculate Distance of Edge ======================================
summary(rs$y)
min(rs$y) #-13.08359 = 1000 meters
max(rs$y) #-13.05419 = 0 meters

diffy = min(rs$y) - max(rs$y)
rs$dist = ((max(rs$y) - rs$y)/diffy)*1000
rs$dist = abs(rs$dist)

rs$dist2 = c("a")
rs$dist2[rs$dist <= 250] = c("Borda")
rs$dist2[rs$dist > 250] = c("Interior")

#Normalize Index values ==========================================
rs_norm <- rs %>%
  group_by(index) %>%
  mutate(value_norm = scale(value)) %>%
  ungroup()

#NDVI ============================================================
mod_NDVI5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI5)

#MSI ============================================================
mod_msi5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "msi"))
summary(mod_msi5)

#NDII ============================================================
mod_ndii5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "ndii"))
summary(mod_ndii5)

#NBR2 ============================================================
mod_nbr25 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "nbr2"))
summary(mod_nbr25)

#RENDVI ============================================================
mod_rendvi5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index== "rendvi"))
summary(mod_rendvi5)

#PSRI ============================================================
mod_psri5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "psri"))
summary(mod_psri5)

#All Results ======================================================
library(ggpubr)

a = plot(ggpredict(mod_NDVI5, terms = c("dist", "year", "treat")))
b = plot(ggpredict(mod_msi5, terms = c("dist", "year", "treat")))
c = plot(ggpredict(mod_ndii5, terms = c("dist", "year", "treat")))
d = plot(ggpredict(mod_nbr25, terms = c("dist", "year", "treat")))
e = plot(ggpredict(mod_rendvi5, terms = c("dist", "year", "treat")))
f = plot(ggpredict(mod_psri5, terms = c("dist", "year", "treat")))

lm_plot = function(w,title){
  w+ggtitle(title)+
    xlab(NULL)+ylab(NULL)+
    #scale_color_manual(values = c("blue", "orange", "red","green","purpple","black","white"))+
    #scale_fill_manual(values = c("blue", "orange", "red","green","purpple","black","white"))+
    theme_minimal()+
    theme(text = element_text(family = "Times New Roman", size = 14))
}

a = lm_plot(a, title = "NDVI")
b = lm_plot(b, title = "MSI")
c = lm_plot(c, title = "NDII")
d = lm_plot(d, title = "NBR2")
e = lm_plot(e, title = "RENDVI")
f = lm_plot(f, title = "PSRI")

preds = ggarrange(a, b, c, d, e, f,
                   common.legend = TRUE,
                   legend="right",
                   ncol = 2, nrow = 3)


ggsave(filename = "LMM_Model5_panel.png", plot = preds,
       path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Models", width = 30, height = 20, units = "cm", dpi = 300)
