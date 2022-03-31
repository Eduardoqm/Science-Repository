#-----------------------------
#Wind Speed (Tower)
#-----------------------------
#Eduardo Q Marques 17-03-2022
#eduardobio2009@gmail.com
#-----------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)

setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

blow = torre

blow$datetime = substr(blow$datetime, 1, 7)

blow = blow %>%
  group_by(datetime) %>%
  #filter(y == 2019 & m < 3 & d < 32) %>% 
  summarise(max_speed = max(max_speed))
#select(datetime, max_speed)

blow$grp = "grp"
blow$datetime = as.Date(blow$datetime)

ggplot(blow, aes(x=datetime, y=(max_speed*3.6)))+
  geom_line(aes(group = grp), size = 1, col = "darkgreen")+
  #geom_point(col = "darkblue", alpha = 0.35)+
  #geom_col(fill = "darkgreen", alpha = 0.7)+
  theme_minimal()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 90, hjust=1), legend.position=c(.20,.75))

ggplot(blow, aes(x=datetime, y=(max_speed*3.6)))+
  geom_line(aes(group = grp), size = 1)+
  geom_point(col = "darkblue", alpha = 0.35)+
  #geom_smooth(col = "black", formula = "mean")+
  geom_col(fill = "darkgreen", alpha = 0.7)+
  theme_minimal()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 0, hjust=1), legend.position=c(.20,.75))
#scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
