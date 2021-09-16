##########################################################
#Linear Mixed Models for VIs from PCA - Model6 (Landsat) #
#                                                        #
#Eduardo Q Marques 15-09-2021                            #
##########################################################

library(tidyverse)
library(reshape2)
library(ggeffects)
library(sjPlot)
library(lattice)
library(lme4)

dir_input <- "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1"

list.files(path = dir_input)

rs <- read_csv(file.path(dir_input, "Landsat_indexs_all_xy.csv"))
rs$treat <- ifelse(rs$treat == "control","aControl", as.character(rs$treat))
rs$treat <- ifelse(rs$treat == "b3yr", "B3yr", as.character(rs$treat))
rs$treat <- ifelse(rs$treat == "b1yr", "B1yr", as.character(rs$treat))

#Modify Data =====================================================
rs$year = substr(rs$year, 1,4)
rs$year = as.numeric(rs$year)
rs$index = as.character(rs$index)
rs$treat = as.character(rs$treat)

rs = rs %>% #Resume repeat years with mean
  group_by(x, y, index, year, treat) %>% 
  summarise(value = mean(value))

#Calculate Distance of Edge ======================================
summary(rs$y)
min(rs$y) #-13.08343 = 1000 meters
max(rs$y) #-13.07422 = 0 meters

diffy = min(rs$y) - max(rs$y)
rs$dist = ((max(rs$y) - rs$y)/diffy)*1000
rs$dist = abs(rs$dist)
summary(rs$dist)

rs$dist2 = c("a")
rs$dist2[rs$dist <= 250] = c("Borda")
rs$dist2[rs$dist > 250] = c("Interior")

#Normalize Index values ==========================================
rs_norm <- rs %>%
  group_by(index) %>%
  mutate(value_norm = scale(value)) %>%
  ungroup()

#NDVI ============================================================
mod_NDVI6 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI6)

#Plot results
plot(ggpredict(mod_NDVI6, terms = c("dist2", "treat")))
plot(ggpredict(mod_NDVI6, terms = c("year", "treat")))
plot(ggpredict(mod_NDVI6, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_NDVI6, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_NDVI6,condVar=TRUE))
sjPlot::plot_model(mod_NDVI6)
sjPlot:: tab_model(mod_NDVI6)

#NDII ============================================================
mod_ndii6 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "ndii"))
summary(mod_ndii6)

#Plot results
plot(ggpredict(mod_ndii6, terms = c("dist2", "treat")))
plot(ggpredict(mod_ndii6, terms = c("year", "treat")))
plot(ggpredict(mod_ndii6, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_ndii6, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_ndii6,condVar=TRUE))
sjPlot::plot_model(mod_ndii6)
sjPlot:: tab_model(mod_ndii6)

#NBR ============================================================
mod_nbr6 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "nbr"))
summary(mod_nbr6)

#Plot results
plot(ggpredict(mod_nbr6, terms = c("dist2", "treat")))
plot(ggpredict(mod_nbr6, terms = c("year", "treat")))
plot(ggpredict(mod_nbr6, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_nbr6, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_nbr6,condVar=TRUE))
sjPlot::plot_model(mod_nbr6)
sjPlot:: tab_model(mod_nbr6)

#NBR2 ============================================================
mod_nbr26 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "nbr2"))
summary(mod_nbr26)

#Plot results
plot(ggpredict(mod_nbr26, terms = c("dist2", "treat")))
plot(ggpredict(mod_nbr26, terms = c("year", "treat")))
plot(ggpredict(mod_nbr26, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_nbr26, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_nbr26,condVar=TRUE))
sjPlot::plot_model(mod_nbr26)
sjPlot:: tab_model(mod_nbr26)

#EVI ============================================================
mod_evi6 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "evi2"))
summary(mod_evi6)

#Plot results
plot(ggpredict(mod_evi6, terms = c("dist2", "treat")))
plot(ggpredict(mod_evi6, terms = c("year", "treat")))
plot(ggpredict(mod_evi6, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_evi6, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_evi6,condVar=TRUE))
sjPlot::plot_model(mod_evi6)
sjPlot:: tab_model(mod_evi6)

#GRND ============================================================
mod_grnd6 <- lmer(value_norm ~ treat*dist2*year + (dist2|year), data = subset(rs_norm, index == "grnd"))
summary(mod_grnd6)

#Plot results
plot(ggpredict(mod_grnd6, terms = c("dist2", "treat")))
plot(ggpredict(mod_grnd6, terms = c("year", "treat")))
plot(ggpredict(mod_grnd6, terms = c("dist2", "treat", "year")))
plot(ggpredict(mod_grnd6, terms = c("year", "treat", "dist2")))

dotplot(ranef(mod_grnd6,condVar=TRUE))
sjPlot::plot_model(mod_grnd6)
sjPlot:: tab_model(mod_grnd6)

#All Results ======================================================
library(ggpubr)

a = plot(ggpredict(mod_grnd6, terms = c("year", "treat", "dist2")))
b = plot(ggpredict(mod_ndii6, terms = c("year", "treat", "dist2")))
c = plot(ggpredict(mod_nbr6, terms = c("year", "treat", "dist2")))
d = plot(ggpredict(mod_nbr26, terms = c("year", "treat", "dist2")))
e = plot(ggpredict(mod_evi6, terms = c("year", "treat", "dist2")))
f = plot(ggpredict(mod_NDVI6, terms = c("year", "treat", "dist2")))

lm_plot = function(w,title){
  w+ggtitle(title)+
    xlab(NULL)+ylab(NULL)+
    scale_color_manual(values = c("blue", "orange", "red"))
}

a = lm_plot(a, title = "GRND")
b = lm_plot(b, title = "NDII")
c = lm_plot(c, title = "NBR")
d = lm_plot(d, title = "NBR2")
e = lm_plot(e, title = "EVI")
f = lm_plot(f, title = "NDVI")

ggarrange(a, b, c, d, e, f,
          common.legend = TRUE,
          legend="right",
          ncol = 2, nrow = 3)






