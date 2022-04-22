#-------------------------------------------------------------
# Circular Trees and Wind Direction from all plot level data
# Blowdown Data (AREA-1)
#
# Eduardo Q Marques 22-04-2022
#-------------------------------------------------------------

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)
library(circular)

#Fall Trees Direction ----------------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

df = read.csv("blowdown_full_update_2021.csv", sep = ",")

#Resume data
df = df %>% 
  filter(caiu._com_vento == "s") %>% 
  select(direction)

df$nt = 1
colnames(df) = c("Direction", "Number_of_Trees")


#Plot fall direction
ggplot(df, aes(x=Direction))+
  geom_density(col = "black", fill = "darkblue", alpha = 0.5)+
  theme_minimal()+
  ggtitle("Direction the trees fell (Degrees)")

wind = df %>% 
  group_by(Direction) %>% 
  summarise(Number_of_Trees = sum(Number_of_Trees)) %>% 
  na.omit()
wind$Direction = as.factor(wind$Direction)

ggplot(wind, aes(x=Direction, y = Number_of_Trees, fill = Number_of_Trees))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())+
  coord_polar(start = 0)+
  ggtitle("Direction the trees fell (Degrees)")


df2 = df[,1] %>% 
  na.omit()
tree_fall <- circular(df2, units = "degrees", template = "none",
                      rotation = "clock", zero = -55) 

plot.circular(tree_fall, stack=T, bg="RoyalBlue", pch=21, cex=1.3,
              main = "Trees falling direction", shrink = 1.5)

arrows.circular(mean(tree_fall), col = "RoyalBlue")


#Wind Diretion Towers -------------------------------------------------------------------------------------------
#Control Tower
control = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres/Raw data/control20152020_gapfilled_jan2022.csv", sep = ",")

control$y = control$Year
control$m =as.numeric(substr(control$DateTime, 6, 7))
control$d = as.numeric(substr(control$DateTime, 9, 10))

crt = control %>%
  filter(y == 2019 & m == 2 & d == 3) %>% 
  select(DateTime, wind_speed, wind_dir)
  
crt2 = crt[,3] %>% 
  na.omit()

crt3 <- circular(crt2, units = "degrees", template = "none",
                      rotation = "clock", zero = -55) 

plot.circular(crt3, stack=T, bg="darkgreen", pch=21, cex=1.3,
              main = "Wind direction (Tower Control)", shrink = 1.5)

arrows.circular(mean(crt3), col = "darkgreen")

#Fire Tower
fire = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres/Raw data/fire20152020_gapfilled_jan2022.csv", sep = ",")

fire$y = fire$Year
fire$m =as.numeric(substr(fire$DateTime, 6, 7))
fire$d = as.numeric(substr(fire$DateTime, 9, 10))

fr = fire %>% 
  filter(y == 2019 & m == 2 & d == 3) %>% 
  select(DateTime, wind_speed, wind_dir)

fr2 = fr[,3] %>% 
  na.omit()

fr3 <- circular(fr2, units = "degrees", template = "none",
                 rotation = "clock", zero = -55) 

plot.circular(fr3, stack=T, bg="red", pch=21, cex=1.3,
              main = "Wind direction (Tower Fire)", shrink = 1.5)

arrows.circular(mean(fr3), col = "red")
  
#Wind Diretion (Darro)
darro = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres/Raw data/Master_Estacao_Darro_2021.csv", sep = ",")

blowd = darro %>% 
  filter(Year == 2019 & Month == 2 & Day == 3) %>% 
  select(Date2, windSpd, windDir)

blowd2 = blowd[,3] %>% 
  na.omit()

windrd <- circular(blowd2, units = "degrees", template = "none",
                  rotation = "clock", zero = -55) 

plot.circular(windrd, stack=T, bg="purple", pch=21, cex=1.3,
              main = "Wind direction (Darro)", shrink = 1.5)

arrows.circular(mean(windrd), col = "purple")  


#All
par(mfrow=c(1,4))
plot.circular(tree_fall, stack=T, bg="RoyalBlue", pch=21, cex=1.5,
              main = "Trees falling direction", shrink = 1)
arrows.circular(mean(tree_fall), col = "RoyalBlue")
  
plot.circular(crt3, stack=T, bg="darkgreen", pch=21, cex=1.5,
              main = "Wind direction (Tower Control)", shrink = 1)
arrows.circular(mean(crt3), col = "darkgreen")

plot.circular(fr3, stack=T, bg="red", pch=21, cex=1.3,
              main = "Wind direction (Tower Fire)", shrink = 1.5)
arrows.circular(mean(fr3), col = "red")
  
plot.circular(windrd, stack=T, bg="purple", pch=21, cex=1.5,
              main = "Wind direction (Darro)", shrink = 1)
arrows.circular(mean(windrd), col = "purple")  

#Compare Tower and Darro stations ----------------------------------------------------------------------
blow$Station = c("Tower")
colnames(blow) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
blowd$Station = c("Darro")
colnames(blowd) = c("Date", "Wind_Speed", "Wind_Direction", "Station")

stations = rbind(blow, blowd)

ggplot(stations, aes(x=Date, y=(Wind_Speed*3.6), col = Station))+
  geom_line(aes(group = Station), size = 1)+
  theme_bw()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position=c(.20,.75))


blow2 = torre %>% 
  select(datetime, max_speed)

ggplot(blow2, aes(x=datetime, y=(max_speed*3.6)))+
  geom_line(aes(group = datetime), size = 1)+
  theme_bw()+
  xlab(NULL)+
  ylab("Wind Speed (km/h)")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position=c(.20,.75))

#===========================================================================================
#Wind Speed (Tower)
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
