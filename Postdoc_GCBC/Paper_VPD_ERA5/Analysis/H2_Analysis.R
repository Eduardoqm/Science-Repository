#Paper VPD ERA5 Amazon - Analysis
# H2 - Along the successional gradient, young secondary forests
#remain exposed for longer periods to microclimatic conditions
#that favor fire spread throughout the dry season.

#Eduardo Q Marques 25-08-2026

library(tidyverse)

#Load data ---------------------------------------------------------------------
setwd("G:/My Drive/Research/PosDoc_GCBC/Dados e Analises/H2")
dir()



#Graphics ----------------------------------------------------------------------
ggplot(df4, aes(x=Age, y=Hours))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(df4, aes(x=Age, y=Hours, col=Month))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month, scale = "free")


