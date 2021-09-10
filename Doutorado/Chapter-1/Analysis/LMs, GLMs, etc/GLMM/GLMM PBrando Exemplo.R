#GLMM PBrando Exemplo

library(tidyverse)

dir_input <- "/Volumes/GoogleDrive/My Drive/Document/Manuscript/Tanguro_RemoteSensingDudu/Dados"

list.files(path = dir_input)

rs <- read_csv(file.path(dir_input, "Hyperion_indexs_all_xy-B_cent.csv"))
rs$Treat <- ifelse(rs$Parcela == "Controle", "AControl", as.character(rs$Parcela))
library(lme4)

###
###
###

rs_norm <- rs %>%
  group_by(index) %>%
  mutate(value_norm = standardize(value)) %>%
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

summary(mod_EVI1)
plot(ggpredict(mod_EVI_norm, terms = c("dist", "Treat")))

sjPlot::plot_model(mod_EVI1)
sjPlot:: tab_model(mod_EVI1)

dotplot(ranef(mod_EVI_norm1,condVar=TRUE))