#------------------------------
# Chi and Chibar plots
#
# Eduardo Q Marques 23-05-2022
#------------------------------

library(tidyverse)
library(ggplot2)
library(ggpubr)

#Unite Tail dependence results from Darro and ERA5 ------------------------------------
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Chi and Chibar")

chi_darro = read.csv("CHI_Darro.csv", sep = ",")
chibar_darro = read.csv("CHIBAR_Darro.csv", sep = ",")

chi_torre = read.csv("CHI_Tower.csv", sep = ","); chi_torre = chi_torre %>% filter(Date != 2019)
chibar_torre = read.csv("CHIBAR_Tower.csv", sep = ",")

chi_era = read.csv("CHI_ERA5.csv", sep = ",")
chibar_era = read.csv("CHIBAR_ERA5.csv", sep = ",")

chi2 = rbind(chi_darro, chi_era, chi_torre)
chibar2 = rbind(chibar_darro, chibar_era, chibar_torre)
colnames(chi2)[6] = c("Dataset")
colnames(chibar2)[6] = c("Dataset")

chi_plot = ggplot(chi2, aes(quant, value, fill = Dataset, linetype = Dataset))+
  geom_line()+
  labs(x = NULL, y = "Chi", title = "A")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
  scale_fill_manual(values =  c("#33a02c", "red", "#1f78b4"))+
  facet_wrap(~Date)+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = c(.9,.2)); chi_plot

chibar_plot = ggplot(chibar2, aes(quant, value, fill = Dataset, linetype = Dataset))+
  geom_line()+
  labs(x = "Quantile theshold q", y = "Chibar", title = "B")+
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.2)+
  scale_fill_manual(values =  c("#33a02c", "red", "#1f78b4"))+
  facet_wrap(~Date)+
  ylim(-1, 1)+
  theme_bw()+
  theme(legend.position = c(.9,.2)); chibar_plot


chis = ggarrange(chi_plot, chibar_plot, ncol = 1); chis


#ggsave(filename = "WS-Prec_chi_chibar_Facet.png", plot = chis,
#      path = "C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Capitulo2/Figuras/Wind Speed vs Precipitation", width = 25, height = 30, units = "cm", dpi = 300)
