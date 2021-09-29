###########################################################
#Linear Mixed Models for VIs from PCA - Model7 (Hyperion) #
#                                                         #
#Eduardo Q Marques 14-09-2021                             #
###########################################################

library(tidyverse)
library(reshape2)
library(ggeffects)
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
max(rs$y) #-13.07419 = 0 meters

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
mod_NDVI7 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI7)

#Plot results
plot(ggpredict(mod_NDVI7, terms = c("dist2", "treat")))
plot(ggpredict(mod_NDVI7, terms = c("year", "treat")))
plot(ggpredict(mod_NDVI7, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_NDVI7, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_NDVI7,condVar=TRUE))
sjPlot::plot_model(mod_NDVI7)
sjPlot::tab_model(mod_NDVI7)

#MSI ============================================================
mod_msi7 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "msi"))
summary(mod_msi7)

#Plot results
plot(ggpredict(mod_msi7, terms = c("dist2", "treat")))
plot(ggpredict(mod_msi7, terms = c("year", "treat")))
plot(ggpredict(mod_msi7, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_msi7, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_msi7,condVar=TRUE))
sjPlot::plot_model(mod_msi7)
sjPlot::tab_model(mod_msi7)


#NDII ============================================================
mod_ndii7 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "ndii"))
summary(mod_ndii7)

#Plot results
plot(ggpredict(mod_ndii7, terms = c("dist2", "treat")))
plot(ggpredict(mod_ndii7, terms = c("year", "treat")))
plot(ggpredict(mod_ndii7, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_ndii7, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_ndii7,condVar=TRUE))
sjPlot::plot_model(mod_ndii7)
sjPlot::tab_model(mod_ndii7)

#NBR2 ============================================================
mod_nbr27 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "nbr2"))
summary(mod_nbr27)

#Plot results
plot(ggpredict(mod_nbr27, terms = c("dist2", "treat")))
plot(ggpredict(mod_nbr27, terms = c("year", "treat")))
plot(ggpredict(mod_nbr27, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_nbr27, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_nbr27,condVar=TRUE))
sjPlot::plot_model(mod_nbr27)
sjPlot::tab_model(mod_nbr27)

#RENDVI ============================================================
mod_rendvi7 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "rendvi"))
summary(mod_rendvi7)

#Plot results
plot(ggpredict(mod_rendvi7, terms = c("dist2", "treat")))
plot(ggpredict(mod_rendvi7, terms = c("year", "treat")))
plot(ggpredict(mod_rendvi7, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_rendvi7, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_rendvi7,condVar=TRUE))
sjPlot::plot_model(mod_rendvi7)
sjPlot::tab_model(mod_rendvi7)

#PSRI ============================================================
mod_psri7 <- lmer(value_norm ~ treat*dist2*year + (treat|year), data = subset(rs_norm, index == "psri"))
summary(mod_psri7)

#Plot results
plot(ggpredict(mod_psri7, terms = c("dist2", "treat")))
plot(ggpredict(mod_psri7, terms = c("year", "treat")))
plot(ggpredict(mod_psri7, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_psri7, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_psri7,condVar=TRUE))
sjPlot::plot_model(mod_psri7)
sjPlot::tab_model(mod_psri7)

#All Results ======================================================
library(ggpubr)

a = plot(ggpredict(mod_NDVI7, terms = c("year", "treat", "dist2")))
b = plot(ggpredict(mod_msi7, terms = c("year", "treat", "dist2")))
c = plot(ggpredict(mod_ndii7, terms = c("year", "treat", "dist2")))
d = plot(ggpredict(mod_nbr27, terms = c("year", "treat", "dist2")))
e = plot(ggpredict(mod_rendvi7, terms = c("year", "treat", "dist2")))
f = plot(ggpredict(mod_psri7, terms = c("year", "treat", "dist2")))

lm_plot = function(w,title){
  w+ggtitle(title)+
    xlab(NULL)+ylab(NULL)+
    scale_color_manual(values = c("blue", "orange", "red"))+
    scale_fill_manual(values = c("blue", "orange", "red"))+
    theme_bw()+
    theme(text = element_text(family = "Times New Roman", size = 14))
}

a = lm_plot(a, title = "NDVI")
b = lm_plot(b, title = "MSI")
c = lm_plot(c, title = "NDII")
d = lm_plot(d, title = "NBR2")
e = lm_plot(e, title = "RENDVI")
f = lm_plot(f, title = "PSRI")

preds =  ggarrange(a, b, c, d, e, f,
          common.legend = TRUE,
          legend="right",
          ncol = 2, nrow = 3)


ggsave(filename = "LMM_Model7_panel.png", plot = preds,
     path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo1/Figuras/Models", width = 30, height = 20, units = "cm", dpi = 300)



