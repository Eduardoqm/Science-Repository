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
library(metan)

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

#Wind Diretion (ERA5 Land)
era = read.csv("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/ERA5_Land/Wind_Direction_ERA5_Land_03-02-2019.csv", sep = ",")

era2 = era[,2] %>% 
  na.omit()

era3 <- circular(era2, units = "degrees", template = "none",
                 rotation = "clock", zero = -55) 

plot.circular(era3, stack=T, bg="orange", pch=21, cex=1.3,
              main = "Wind direction (ERA5 Land)", shrink = 1.5)

arrows.circular(mean(windrd), col = "orange")

#Compare Tower and Darro stations ----------------------------------------------------------------------------
crt$Station = c("Control")
colnames(crt) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
crt$Time = substr(crt$Date, 12, 16)

fr$Station = c("Fire")
colnames(fr) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
fr$Time = substr(fr$Date, 12, 16)

blowd$Station = c("Darro")
colnames(blowd) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
blowd$Time = substr(blowd$Date, 12, 16)

era$Station = c("ERA5_Land")
era = era[,c(1,3,2,4)]
colnames(era) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
era$Time = substr(era$Date, 12, 16)

#Wind Direction
wdir = cbind(crt[,c(5,3)], fr[,3], blowd[,3])

wdir = full_join(wdir, era, by = "Time")
wdir = wdir[,c(-5, -6, -8)]

colnames(wdir) = c("Time", "Control", "Fire", "Darro", "ERA5_Land")

corr_plot(wdir,
          shape.point = 21,
          col.point = "black",
          fill.point = "orange",
          size.point = 5,
          alpha.point = 0.6,
          maxsize = 4,
          minsize = 2,
          smooth = TRUE,
          col.smooth = "black",
          col.sign = "cyan",
          upper = "scatter",
          lower = "corr",
          diag.type = "density",
          col.diag = "cyan",
          pan.spacing = 0,
          lab.position = "bl")+
  labs(title = "Wind Directiom (degrees)")


#Wind Speed
stations = rbind(crt, fr, blowd, era)

ggplot(stations, aes(x=Time, y=Wind_Speed, col = Station))+
  geom_line(aes(group = Station), size = 1.5)+
  theme_bw()+
  xlab(NULL)+
  ylab("Mean Wind Speed (m/s)")+
  scale_color_manual(values = c("darkgreen", "purple", "orange", "red"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position=c(.20,.75))


ws = cbind(crt[,c(5,2)], fr[,2], blowd[,2])
ws = full_join(ws, era, by = "Time")
ws = ws[,c(-5, -7, -8)]

colnames(ws) = c("Time", "Control", "Fire", "Darro", "ERA5_Land")

corr_plot(ws,
          shape.point = 21,
          col.point = "black",
          fill.point = "orange",
          size.point = 5,
          alpha.point = 0.6,
          maxsize = 4,
          minsize = 2,
          smooth = TRUE,
          col.smooth = "black",
          col.sign = "cyan",
          upper = "scatter",
          lower = "corr",
          diag.type = "density",
          col.diag = "cyan",
          pan.spacing = 0,
          lab.position = "bl")+
  labs(title = "Wind Speed (m/s)")






