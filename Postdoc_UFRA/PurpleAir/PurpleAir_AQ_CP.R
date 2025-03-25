#Graphic CP-PA Air Quality

#Eduardo Q Marques  14-10-2024

library(ggplot2)
library(data.table)
library(lubridate)
library(tidyverse)

#Load CP data ------------------------------------------------------------------
setwd("C:/Users/Workshop/Documents/Research/PurpleAir/CP_Data")
#dir()

lista <- list.files(pattern = ".csv$", full.names=TRUE,recursive=TRUE)

CP=read.csv(lista[1])

for (x in 2:length(lista)) {
  print(lista[x])
  CPx = read.csv(lista[x])
  CP = rbind(CP, CPx)
}

CP$Date = as.Date(substr(CP$UTCDateTime, 1,10), "%Y/%m/%d")
CP$Hours = substr(CP$UTCDateTime, 12,13)
CP2 = CP %>% 
  filter(current_humidity > 10) %>% 
  filter(current_temp_f > 10) %>% 
  filter(pressure > 900)

#Filter variables --------------------------------------------------------------
#df = rbind(CP, Darro)

#PM25
pm25 = CP2 %>%
  group_by(Date, Hours)%>%
  summarise(Value = mean(pm2.5_aqi_atm, na.rm=T))%>%
  group_by(Date, Hours)

#pm25$var = c("Air Quality (μg/m³)")
pm25$var = c("Qualidade do ar (μg/m³)")

#Temperature
temp = CP2 %>%
  group_by(Date, Hours)%>%
  summarise(Value = mean(current_temp_f, na.rm=T))%>%
  group_by(Date, Hours)

temp$var = c("Temperatura (°C)")

temp$Value = ((temp$Value - 32) * 5/9)
temp$Value[temp$Value > 60] = NA
temp$Value[temp$Value < 0] = NA
temp %>% na.omit()

#Humidity
hum = CP2 %>%
  group_by(Date, Hours)%>%
  summarise(Value = mean(current_humidity, na.rm=T))%>%
  group_by(Date, Hours)

#hum$var = c("Humidity (%)")
hum$var = c("Umidade do ar (%)")

hum$Value[hum$Value >100] = NA
hum %>% na.omit()

#Pressure
pres = CP2 %>%
  group_by(Date, Hours)%>%
  summarise(Value = mean(pressure, na.rm=T))%>%
  group_by(Date, Hours)

#pres$var = c("Pressure (hPa)")
pres$var = c("Pressão atmosférica (hPa)")

pres$Value[pres$Value < 750] = NA
pres %>%  na.omit()

#Join every variable -----------------------------------------------------------
df = rbind(pm25, temp, hum, pres)

df2 = df %>% 
  filter(Date > "2022-07-14")

plot1 = ggplot(df2, aes(x = Date,y = Value))+
  geom_point(size = 3, alpha = 0.15, col = "purple")+
  geom_smooth(se=F,span=0.1,alpha=0.2, col = "black")+
  labs(x = NULL, y = NULL)+
  #scale_colour_manual(values = c("darkgreen", "darkorange"))+
  facet_wrap(~var, scale = "free")+
  theme_bw(base_size = 15); plot1


ggsave(filename = "PurpleAir_CP.png", plot = plot1,
       path = "C:/Users/Workshop/Documents/Research/PurpleAir",
       width = 25, height = 15, units = "cm", dpi = 300)


#Air Quality graph -------------------------------------------------------------
plot2 = ggplot(pm25, aes(x = Date,y = Value))+
  geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                ymin = 0, ymax = 12), col = "#ccff99", fill = "#ccff99")+
  geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                ymin = 12, ymax = 35), col = "#ffffcc", fill = "#ffffcc")+
  geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                ymin = 35, ymax = 55), col = "#ffebcc", fill = "#ffebcc")+
  geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                ymin = 55, ymax = 100), col = "#ffcccc", fill = "#ffcccc")+
  #geom_rect(aes(xmin = as.Date("2021-08-01"), xmax = as.Date("2023-11-01"),
   #             ymin = 150, ymax = 175), col = "#e6ccff", fill = "#e6ccff")+
  #geom_rect(aes(xmin = as.Date("2021-09-22"), xmax = as.Date("2023-11-01"),
  #              ymin = 250, ymax = 500), col = "#ffddcc", fill = "#ffddcc")+
  #geom_point(size = 3, alpha = 0.15)+
  #geom_smooth(se=F, alpha=0.2)+
  geom_line()+
  geom_hline(yintercept=c(0, 12, 35, 55),
             linetype="dashed", color = "darkgray")+
 # annotate("text", x = as.Date("2021-08-01"),
  #         y = c(7, 21, 44, 64, 160),
   #        label= c("good", "moderate",
    #                "unhealthy for sensitive groups",
     #               "unhealthy", "very unhealthy"),
      #     color = "darkgray", hjust = "inward")+
  annotate("text", x = as.Date("2022-01-01"),
           y = c(7, 21, 44, 64),
           label= c("Bom", "Moderado",
                    "Insalubre para
grupos de risco",
                    "Insalubre"),
           color = "darkgray", hjust = "inward")+
  #labs(x = NULL, y = "Air Quality (μg/m³)", colour = NULL)+
  labs(x = NULL, y = "Qualitdade do ar (μg/m³)", colour = NULL)+
  #scale_colour_manual(values = c("darkgreen", "darkorange"))+
  #facet_grid(rows = vars(Site), scale = "free")+
  theme_bw(base_size = 15)+
  theme(legend.position = c(0.75, 0.8),
        legend.background = element_rect(fill="#e6ccff", colour ="#e6ccff")); plot2

ggsave(filename = "AQ_CP.png", plot = plot2,
       path = "C:/Users/Workshop/Documents/Research/PurpleAir",
       width = 25, height = 15, units = "cm", dpi = 300)






#Making a GIF ------------------------------------------------------------------
pm25b = pm25[1,]

for (z in 2:length(pm25$Date)) {
  #Air Quality graph -----------------------------------------------------------
  print(z)
  pm25b = rbind(pm25b, pm25[z,])
  
  gif = ggplot(pm25b, aes(x = Date,y = Value))+
    geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                  ymin = 0, ymax = 12), col = "#ccff99", fill = "#ccff99")+
    geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                  ymin = 12, ymax = 35), col = "#ffffcc", fill = "#ffffcc")+
    geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                  ymin = 35, ymax = 55), col = "#ffebcc", fill = "#ffebcc")+
    geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2024-09-01"),
                  ymin = 55, ymax = 100), col = "#ffcccc", fill = "#ffcccc")+

    geom_line()+
    geom_hline(yintercept=c(0, 12, 35, 55),
               linetype="dashed", color = "darkgray")+
  
    annotate("text", x = as.Date("2022-01-01"),
             y = c(7, 21, 44, 64),
             label= c("Bom", "Moderado",
                      "Insalubre para
grupos de risco",
                      "Insalubre"),
             color = "darkgray", hjust = "inward")+
    #labs(x = NULL, y = "Air Quality (μg/m³)", colour = NULL)+
    labs(x = NULL, y = "Qualitdade do ar (μg/m³)", colour = NULL)+

    theme_bw(base_size = 15)+
    theme(legend.position = c(0.75, 0.8),
          legend.background = element_rect(fill="#e6ccff", colour ="#e6ccff"))
  
  ggsave(filename = paste0(z, "AQ_CP.png"), plot = gif,
         path = "C:/Users/Workshop/Documents/Research/PurpleAir/gif",
         width = 25, height = 15, units = "cm", dpi = 300)
}


#PurpleAir variables -----------------------------------------------------------
df2b = rbind(pm25[2,], temp[2,], hum[2,], pres[2,])


for (z in 3:length(pm25$Date)) {
  print(z)
  df2b = rbind(df2b, pm25[z,], temp[z,], hum[z,], pres[z,])
  
  gif2 = ggplot(df2b, aes(x = Date,y = Value))+
    geom_point(size = 3, alpha = 0.15, col = "purple")+
    geom_smooth(se=F,span=0.1,alpha=0.2, col = "black")+
    
    #geom_vline(xintercept = as.Date("2024-09-01"),
    #           linetype = "dashed", color = "darkgray")+
    
    xlim(as.Date("2022-07-14"), as.Date("2024-09-01"))+
    
    labs(x = NULL, y = NULL)+
    facet_wrap(~var, scale = "free")+
    theme_bw(base_size = 15)
  
  
  ggsave(filename = paste0(z, "Vars_PurpleAir_CP.png"), plot = gif2,
         path = "C:/Users/Workshop/Documents/Research/PurpleAir/gif",
         width = 25, height = 15, units = "cm", dpi = 300)
  
}






