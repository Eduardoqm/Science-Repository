#Eco Services Scenarios Biomass
#Eduardo Q Marques 17-08-2026

library(terra)
library(tidyverse)

setwd("/home/leaf/Documentos/Serrapilheira _Elias et al/Scenery")
dir()

#Load data ---------------------------------------------------------------------
esa = rast("ESA_Biomass_70m.tif"  )
scf = rast("MB_Forest_age_70m.tif")
past = rast("Pasture_70m.tif")
prf = rast("Forest_70m.tif")
dlt_esa_p = rast("Delta_AGB_Forest_age_Pasture.tif")
df_delta = read.csv("Pasture_AGB_age_full_C.csv")

plot(esa)
plot(dlt_esa_p)


#Scenery 1 -> Coverting SF in Pasture ------------------------------------------
dlt_esa_p2 = ifel(is.na(scf), NA, dlt_esa_p) #Filtering SF
plot(dlt_esa_p2)

dlt_esa_p3 = dlt_esa_p2*-1 #Inverting signal to make sense
plot(dlt_esa_p3)

#Scenery 1
scn_p = esa+dlt_esa_p3
scn_p2 = ifel(is.na(scn_p), esa, scn_p)

mean(values(scn_p2),na.rm=T) #204.06
mean(values(esa),na.rm=T) #206.02

#Result
204.06 - 206.02 #-1.95
#Converting SF in Pasture decrease the AGB in 1.95 tons per pixel (3.98t/ha).


#Scenery 2 and 3-> Converting Pasture in sf and persisting to 38 years ---------
df_delta2 = df_delta %>%
  group_by(age) %>% 
  summarise(delta_esa = mean(delta_esa))

#Input min, mean and maximun
max_lmar = min(df_delta2$delta_esa) #Delta in 38 years (oldest SF)
mean_lmar = mean(df_delta2$delta_esa) #Mean delta
min_lmar = max(df_delta2$delta_esa) #Delta for youngster SF

past_esa = ifel(is.na(past), NA, esa) #Filtering Pasture pixels

#Delta for youngster SF
scn_max_sf = past_esa+max_lmar
scn_max_sf2 = ifel(is.na(scn_max_sf), esa, scn_max_sf)

mean(values(scn_max_sf2),na.rm=T) #199.47

#Result
199.47 - 206.02 #-0.92
#Converting Pasture to young SF, the AGB decrease 13.47 tons per pixel (27.49/ha).

#Mean delta
scn_mean_sf = past_esa+mean_lmar
scn_mean_sf2 = ifel(is.na(scn_mean_sf), esa, scn_mean_sf)

mean(values(scn_mean_sf2),na.rm=T) #215.29

#Result
215.29 - 206.02 #9.27
#Converting Pasture to SF, in mean the AGB increase 9.27 tons per pixel (18.92/ha).


#Mean delta
scn_min_sf = past_esa+min_lmar
scn_min_sf2 = ifel(is.na(scn_min_sf), esa, scn_min_sf)

mean(values(scn_min_sf2),na.rm=T) #219.49

#Result
219.49 - 206.02 # 13.47
#Converting Pasture to 38 year old SF, the AGB increase 13.47 tons per pixel (27.49/ha).

#Sum Scenery results -----------------------------------------------------------
#expanse(scf, unit = "m")
#Area of SF is 81,000,000,000 m² (MapBiomas)

#expanse(past, unit = "m")
#Area of pasture is 508,281,750,000 m² (MapBiomas)

#Pixel area is 4900 m²

#Scenary 1
(-1.95*81000000000)/4900  #-32234694 tons
#Converting SF in Pasture decrease the AGB in a total of −0.03223 Pg.

#Scenary 2
(9.27*508281750000)/4900  #961586086 tons
#Converting Pasture to SF, in mean the AGB increase a total of 0.962 Pg.

(13.47*508281750000)/4900  #1397256158 tons
#Converting Pasture to 38 year old SF, the AGB increase increase a total of  1.397 Pg.

#Saving rasters of scenarios ---------------------------------------------------
writeRaster(scn_p2, "Scenary_ESA_SF_to_Pasture.tif")
writeRaster(scn_min_sf2, "Scenary_ESA_Pasture_to_young_SF.tif")
writeRaster(scn_mean_sf2, "Scenary_ESA_Pasture_to_mean_SF.tif")
writeRaster(scn_max_sf2, "Scenary_ESA_Pasture_to_old_SF.tif")





#Elias Scenary Graphs ----------------------------------------------------------
### Figuras dos cenarios

library(tidyverse)
library(patchwork)
require(ggpubr)

df <- tibble(
  Scenario = factor(
    c(
      "SF → Pasture",
      "Pasture → SF",
      "Young SF protected\nfor 20 years\n(38-year-old SF)"
    ),
    levels = c(
      "SF → Pasture",
      "Pasture → SF",
      "Young SF protected\nfor 20 years\n(38-year-old SF)"
    )
  ),
  LST = c(
    0.05,
    -0.65,
    -0.90
  ),
  ET = c(
    -0.01,
    0.06,
    0.11
  ),
  AGB = c(
    -0.03223,
    0.962,
    1.397
  )
)

#---------------------------------
# Colors
#---------------------------------

cols <- c(
  "SF → Pasture" = "#C65A5A",
  "Pasture → SF" = "#66BD63",
  "Young SF protected\nfor 20 years\n(38-year-old SF)" = "#1A9850"
)

#---------------------------------
# Panel A - LST
#---------------------------------

p1 <- ggplot(df, aes(x = Scenario,
                 y = LST,
                 fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(
    aes(label = sprintf("%.2f", LST)),
    hjust = ifelse(df$LST > 0, -0.25, 1.20),
    size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(-1.0, 0.15),
    breaks = seq(-1.0, 0.1, by = 0.2),
    expand = expansion(mult = c(0.02, 0.08))) +
  
  labs(
    x = NULL,
    y = expression(Delta*"LST (degree*C)")) +
  
  theme_classic(base_size = 16) +
  
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank())

#---------------------------------
# Panel B - ET
#---------------------------------

p2 <- ggplot(df, aes(x = Scenario,
                 y = ET,
                 fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(aes(label = sprintf("%.2f", ET)),
    hjust = ifelse(df$ET > 0, -0.25, 1.20),
    size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(-0.06, 0.13),
    breaks = seq(-0.06, 0.12, by = 0.03),
    expand = expansion(mult = c(0.02, 0.08))) +
  
  labs(
    x = NULL,
    y = expression(Delta*"ET (mm day"^{-1}*")")) +
  
  theme_classic(base_size = 16) +
  
  theme(legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank())

#---------------------------------
# Panel C - ABG
#---------------------------------

p3 <- ggplot(df, aes(x = Scenario,
                 y = AGB,
                 fill = Scenario)) +
  
  geom_col(width = 0.7) +
  
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             colour = "grey40") +
  
  geom_text(aes(label = sprintf("%.2f", AGB)),
    hjust = ifelse(df$AGB > 0, -0.25, 1.20),
    size = 4.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = cols) +
  
  scale_y_continuous(limits = c(-0.1, 1.7),
    #breaks = seq(-0.06, 0.12, by = 0.03),
    #expand = expansion(mult = c(0.02, 0.08))
    ) +
  
  labs(
    x = NULL,
    y = expression("AGB (Pg)")) +
  
  theme_classic(base_size = 16) +
  
  theme(legend.position = "none",
    axis.text.y = element_text(size = 12),
    axis.title.y = element_blank())
#---------------------------------
# Combine panels
#---------------------------------
fig <- ggarrange(p1, p2, p3, 
                 ncol = 1,
                 nrow = 3,
                 labels = c('a)', 'b)', 'c)'),   
                 label.x = 0.01, 
                 align = c("v"),
                 common.legend = F); fig
