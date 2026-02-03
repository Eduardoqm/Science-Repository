#Forest Area by Olofsson Test

#Eduardo Q Marques 08-10-2024 Updated: 03-02-2026

library(tidyverse)

#Load Classes Data -------------------------------------------------------------
setwd("G:/My Drive/Research/Papers/Amazonia_validation (Marques et al)/Accuracy_Test/V4")
dir()

df = read.csv("Olofsson_area.csv")
df$Area_km = df$Area_perc*4196943
df$se_km = df$Sea*4196943
df$Area_perc = df$Area_perc*100
df$Area_km2 = round(df$Area_km, 0)
df$Area_perc = round(df$Area_perc, 1)


plt1 = ggplot(df, aes(x = Area_km2, y = reorder(Class, Area_perc, sum)))+
  geom_bar(position = "stack", stat = "identity",
           fill =  "royalblue", alpha = 0.7)+
  geom_errorbar(aes(xmin = Area_km2 + (0 - se_km), xmax = Area_km2 + se_km),
                width = 0.3, col = "darkblue")+
  labs(x = "Area (km²)", y = NULL)+
  xlim(0,3500000)+
  geom_text(aes(label = paste0(Area_perc, "%")),
            position = position_dodge(width = 0.9),
            vjust = 0.2, hjust = -0.3)+

  theme_minimal(); plt1


#Saving
setwd("G:/My Drive/Postdoc_UFRA/Papers/Amazonia_validation (Marques et al)/Figures")

ggsave(filename = "Area_Olofsson.tiff", plot = plt1,
       width = 19, height = 10, units = "cm", dpi = 300)


