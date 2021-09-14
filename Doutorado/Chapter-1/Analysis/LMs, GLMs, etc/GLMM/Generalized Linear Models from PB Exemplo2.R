#Generalized Linear Models from PB Exemplo

#Eduardo Q Marques 14-09-2021



library(tidyverse)
library(reshape2)
library(ggeffects)
library(sjPlot)
library(lattice)
library(lme4)

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
mod_NDVI <- lmer(value_norm ~ treat*dist + (1|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI)

mod_NDVI2 <- lmer(value_norm ~ treat*dist + (dist|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI2)

mod_NDVI3 <- lmer(value_norm ~ treat+dist + (dist|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI3)

mod_NDVI4 <- lmer(value_norm ~ treat*year + (year|dist), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI4)

mod_NDVI5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI5)

mod_NDVI5 <- lmer(value_norm ~ treat*dist*year + (dist|year), data = subset(rs_norm, index == "ndvi"))
summary(mod_NDVI5)

#Find the best model ===============================================
anova(mod_NDVI, mod_NDVI2, mod_NDVI3, mod_NDVI4, mod_NDVI5)


plot(ggpredict(mod_NDVI, terms = c("dist", "treat")))
plot(ggpredict(mod_NDVI2, terms = c("dist", "treat")))
plot(ggpredict(mod_NDVI3, terms = c("dist", "treat")))
plot(ggpredict(mod_NDVI4, terms = c("year", "treat")))

#Model-5
plot(ggpredict(mod_NDVI5, terms = c("dist", "treat")))
plot(ggpredict(mod_NDVI5, terms = c("year", "treat")))
plot(ggpredict(mod_NDVI5, terms = c("year", "treat", "dist")))
plot(ggpredict(mod_NDVI5, terms = c("dist", "treat", "year")))



sjPlot::plot_model(mod_NDVI)
sjPlot:: tab_model(mod_NDVI)

sjPlot::plot_model(mod_NDVI2)
sjPlot:: tab_model(mod_NDVI2)

sjPlot::plot_model(mod_NDVI3)
sjPlot:: tab_model(mod_NDVI3)

sjPlot::plot_model(mod_NDVI4)
sjPlot:: tab_model(mod_NDVI4)

sjPlot::plot_model(mod_NDVI5)
sjPlot:: tab_model(mod_NDVI5)


dotplot(ranef(mod_NDVI,condVar=TRUE))
dotplot(ranef(mod_NDVI2,condVar=TRUE))
dotplot(ranef(mod_NDVI3,condVar=TRUE))
dotplot(ranef(mod_NDVI4,condVar=TRUE))
dotplot(ranef(mod_NDVI5,condVar=TRUE))


#MSI ============================================================














