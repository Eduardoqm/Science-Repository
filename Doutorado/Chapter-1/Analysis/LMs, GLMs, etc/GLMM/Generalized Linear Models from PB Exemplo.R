#Generalized Linear Models from PB Exemplo

#Eduardo Q Marques 10-09-2021



library(tidyverse)
library(reshape2)
#library(ggiraphExtra)
library(ggeffects)
library(sjPlot)
library(lattice)

dir_input <- "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Dados para analise cap1"

list.files(path = dir_input)

rs <- read_csv(file.path(dir_input, "Hyperion_indexs_all_xy-B_cent.csv"))
rs$Treat <- ifelse(rs$Parcela == "Controle", "AControl", as.character(rs$Parcela))
library(lme4)

###
###
###

rs_norm <- rs %>%
  group_by(index) %>%
  mutate(value_norm = scale(value)) %>%
  ungroup()

###
###
###

mod_EVI_norm0 <- lmer(value_norm ~ Treat*dist + (1|year), 
                      data = subset(rs_norm, index == "PSSR"))
mod_EVI_norm1 <- lmer(value_norm ~ Treat*dist + (dist|year), 
                      data = subset(rs_norm, index == "PSSR"))


mod_EVI1 <- lmer(value ~ Treat*dist + (1|year), 
                 data = subset(rs_norm, index == "PSSR"))

summary(mod_EVI_norm0)
plot(ggpredict(mod_EVI_norm0, terms = c("dist", "Treat")))

sjPlot::plot_model(mod_EVI_norm0)
sjPlot:: tab_model(mod_EVI_norm0)

dotplot(ranef(mod_EVI_norm0,condVar=TRUE))





#Models ANOVA (AIC) ======================================================================== 
anova(mod_EVI_norm0, mod_EVI_norm1)









