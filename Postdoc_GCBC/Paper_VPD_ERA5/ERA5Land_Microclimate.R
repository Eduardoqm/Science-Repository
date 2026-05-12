#Time Series Microclimate ERA5 Hourly Capoeira UFRA

#Eduardo Q Marques 13-11-2025

library(tidyverse)
library(ggpubr)

#Load data ---------------------------------------------------------------------
setwd("G:/Meu Drive/Research/PosDoc_GCBC/Analises/ERA5") #Ecostation
#setwd("G:/My Drive/Research/PosDoc_GCBC/Analises/ERA5") #Laptop
dir()

era = read.csv("ERA5_Point_Temp_RH_VPD_Precip_1965_2025.csv")
era = era[,-7]
head(era)

#Extracting time variables
era$hour = as.numeric(substr(era$system.index, 10, 11))
era$day = as.numeric(substr(era$system.index, 7, 8))
era$month = as.numeric(substr(era$system.index, 5, 6))
era$Year = as.numeric(substr(era$system.index, 1, 4))

era$month2 = as.character(month(ymd(010101) + months(era$month-1),label=T,abbr=T))

#filterirng months and climate variables ---------------------------------------
era_m = era %>% 
  group_by(hour, month2) %>% 
  summarise(VPD = mean(VPD),
            RH = mean(RH),
            Temp_C = mean(Temp_C),
            precip_mm = mean(precip_mm))

write.csv(era_m, "ERA5_1965-2025_Capoeira_UFRA.csv", row.names = F)

#Plotting hourly variation -----------------------------------------------------
gvpd = ggplot(era_m, aes(x=hour, y=VPD, col=month2))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  geom_hline(aes(yintercept=0.75), colour="black", linetype="dashed")+
  labs(title = "a) ERA5 Land 1965 - 2025",
       x = "Hour", y = "VPD (kPa)", col = "Months")+
  scale_color_discrete(breaks=c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")); gvpd

grh = ggplot(era_m, aes(x=hour, y=RH, col=month2))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "b) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Relative Humidity (%)", col = "Months")+
  scale_color_discrete(breaks=c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")); grh

gtmp = ggplot(era_m, aes(x=hour, y=Temp_C, col=month2))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "c) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Temperature (ºC)", col = "Months")+
  scale_color_discrete(breaks=c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")); gtmp

gprec = ggplot(era_m, aes(x=hour, y=precip_mm, col=month2))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "d) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Precipitation (mm)", col = "Months")+
  scale_color_discrete(breaks=c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")); gprec


clim = ggarrange(gvpd, grh, gtmp, gprec,
                 common.legend = T, legend = "right"); clim

#ggsave(filename = "ERA5_1965-2025_Capoeira_UFRA.png", plot = clim,
#       path = "G:/Meu Drive/Research/PosDoc_GCBC/Analises/Figuras",
#       width = 27, height = 17, units = "cm", dpi = 300)


#Hours > 0.75 kPa --------------------------------------------------------------
vpd_h = era_m[,c(1:3)]
vpd_h$VPD = ifelse(vpd_h$VPD >= 0.75, 1, 0)
vpd_h = vpd_h %>% 
  group_by(month2) %>% 
  summarise(risk_h = sum(VPD))

hi_vpd = ggplot(vpd_h, aes(x=month2, y=risk_h))+
  geom_col(fill = "royalblue", alpha = 0.8)+
  geom_text(aes(label = risk_h), size = 5, col = "white",
            position = position_stack(vjust = 0.9))+
  labs(x = NULL, y = "Hours with VPD >0.75 kPa")+
  scale_x_discrete(limits = c( "Jan","Feb","Mar","Apr","May","Jun",
                               "Jul","Aug","Sep","Oct","Nov","Dec")); hi_vpd

ggsave(filename = "ERA5_Hours_higth_VPD_Capoeira_UFRA.png", plot = hi_vpd,
       path = "G:/Meu Drive/Research/PosDoc_GCBC/Analises/Figuras",
       width = 20, height = 10, units = "cm", dpi = 300)













#Temporal analysis -------------------------------------------------------------
#Looking at how extreme years change in relation to the average
era_yr = era %>% 
  #filter(month2 == "Oct") %>% 
  filter(year %in% c(1965,1970,1975,1980,1985,1990,1995,
                       2000,2005,2010,2015,2020,2025)) %>% 
  group_by(hour, year) %>%
  summarise(VPD = mean(VPD),
            RH = mean(RH),
            Temp_C = mean(Temp_C),
            precip_mm = mean(precip_mm))

ggplot(era_yr, aes(x=hour, y=VPD, col=as.character(year)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  geom_hline(aes(yintercept=0.75), colour="black", linetype="dashed")+
  labs(title = "a) ERA5 Land 1965 - 2025",
       x = "Hour", y = "VPD (kPa)", col = "Year")

ggplot(era_yr, aes(x=hour, y=RH, col=as.character(year)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "b) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Relative Humidity (%)", col = "Year")

ggplot(era_yr, aes(x=hour, y=Temp_C, col=as.character(year)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "c) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Temperature (ºC)", col = "Year")


ggplot(era_yr, aes(x=hour, y=precip_mm, col=as.character(year)))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(se = F)+
  labs(title = "d) ERA5 Land 1965 - 2025",
       x = "Hour", y = "Precipitation (mm)", col = "Year")












# ============================================================
# Annual analysis - Extreme years vs mean
# ============================================================

# Annual means ------------------------------------------------
era_y <- era %>%
  group_by(Year) %>%
  summarise(
    VPD = mean(VPD, na.rm = TRUE),
    RH = mean(RH, na.rm = TRUE),
    Temp_C = mean(Temp_C, na.rm = TRUE),
    precip_mm = sum(precip_mm, na.rm = TRUE)
  )

# Reference mean and SD ---------------------------------------
media_ref <- era_y %>%
  summarise(
    VPD_mean = mean(VPD), VPD_sd = sd(VPD),
    RH_mean = mean(RH), RH_sd = sd(RH),
    Temp_mean = mean(Temp_C), Temp_sd = sd(Temp_C),
    Prec_mean = mean(precip_mm), Prec_sd = sd(precip_mm)
  )

# Classify extreme years --------------------------------------
era_y <- era_y %>%
  mutate(
    VPD_class = case_when(
      VPD > media_ref$VPD_mean + media_ref$VPD_sd ~ "High VPD year",
      VPD < media_ref$VPD_mean - media_ref$VPD_sd ~ "Low VPD year",
      TRUE ~ "Normal"
    ),
    Temp_class = case_when(
      Temp_C > media_ref$Temp_mean + media_ref$Temp_sd ~ "Hot year",
      Temp_C < media_ref$Temp_mean - media_ref$Temp_sd ~ "Cool year",
      TRUE ~ "Normal"
    ),
    Prec_class = case_when(
      precip_mm > media_ref$Prec_mean + media_ref$Prec_sd ~ "Wet year",
      precip_mm < media_ref$Prec_mean - media_ref$Prec_sd ~ "Dry year",
      TRUE ~ "Normal"
    )
  )

# Plot time series with extremes -------------------------------
g_vpd_year <- ggplot(era_y, aes(x = Year, y = VPD, color = VPD_class)) +
  geom_line(color = "grey70") +
  geom_point(size = 3) +
  geom_hline(yintercept = media_ref$VPD_mean, linetype = "dashed") +
  scale_color_manual(values = c("High VPD year" = "red", 
                                "Normal" = "grey40", 
                                "Low VPD year" = "blue")) +
  labs(title = "VPD annual variation (1965–2025)",
       y = "VPD (kPa)", x = "Year", color = "Year type") +
  theme_bw()

g_temp_year <- ggplot(era_y, aes(x = Year, y = Temp_C, color = Temp_class)) +
  geom_line(color = "grey70") +
  geom_point(size = 3) +
  geom_hline(yintercept = media_ref$Temp_mean, linetype = "dashed") +
  scale_color_manual(values = c("Hot year" = "red", 
                                "Cool year" = "blue", 
                                "Normal" = "grey40")) +
  labs(title = "Temperature annual variation (1965–2025)",
       y = "Temperature (ºC)", x = "Year", color = "Year type") +
  theme_bw()

g_prec_year <- ggplot(era_y, aes(x = Year, y = precip_mm, color = Prec_class)) +
  geom_line(color = "grey70") +
  geom_point(size = 3) +
  geom_hline(yintercept = media_ref$Prec_mean, linetype = "dashed") +
  scale_color_manual(values = c("Dry year" = "orange", 
                                "Wet year" = "blue", 
                                "Normal" = "grey40")) +
  labs(title = "Precipitation annual variation (1965–2025)",
       y = "Precipitation (mm)", x = "Year", color = "Year type") +
  theme_bw()

# Combine plots -----------------------------------------------
clim_years <- ggarrange(g_vpd_year, g_temp_year, g_prec_year,
                        ncol = 1, nrow = 3, legend = "right")
clim_years

# ============================================================
# Optional: identify critical years (e.g. High VPD + Dry)
# ============================================================
extremos_conjuntos <- era_y %>%
  filter(VPD_class == "High VPD year" & Prec_class == "Dry year")

print(extremos_conjuntos)


