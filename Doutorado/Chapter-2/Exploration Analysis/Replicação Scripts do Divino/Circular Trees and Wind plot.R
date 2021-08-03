#Circular Trees and Wind
#Blowdown Data (AREA-1)
#Eduardo Q Marques 03-08-2021

library(tidyverse)
library(reshape2)
library(ggplot2)
library(fmsb)

#Load data ------------------------------------------------------------------------------------------
setwd('C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Campo vento')

#df = read.csv("blowdown_full_update_2021.csv", sep = ",")
df = read.csv("blowdown_full_update_2021_B.csv", sep = ",")

#Resume data
#df = df[,c(2,7,8,9,10,11,12,13,14,15,22,23)]
df = df[,c(3,7,8,9,10,11,12,13,14,15,25,18)]

df$nt = 1
colnames(df) = c("Specie","Treatment","Line","Transect","Condition","Alt_Scar","Wind?","Direction","Damage","Alt_Broken","Alt_tree", "DAP", "Number_of_Trees")


#Summary information
summary(df)

#Plot data ------------------------------------------------------------------------------------------
#Fall direction
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




library(circular)
df2 = df[,8] %>% 
  na.omit()
tree_fall <- circular(df2, units = "degrees", template = "none",
                      rotation = "clock", zero = -55) 

plot.circular(tree_fall, stack=T, bg="RoyalBlue", pch=21, cex=1.3,
              main = "Trees falling direction", shrink = 1.5)

arrows.circular(mean(tree_fall), col = "RoyalBlue")


library(sharpshootR)

aspect.plot(df2, q=0.5, stack = T,
            p.bw=30,p.axis = seq(0, 350, by = 30),
            pch=21, col='black', bg='RoyalBlue',cex=1.3,
            arrow.lwd=2, main = "Trees falling direction")





#Wind Diretion (Tower)
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

torre = read.csv("Dados_Vento_Torre_Controle.csv", sep = ",")

blow = torre %>% 
  filter(y == 2019 & m == 2 & d == 3) %>% 
  select(datetime, max_speed, wind_dir)
  
blow2 = blow[,3] %>% 
  na.omit()

windr <- circular(blow2, units = "degrees", template = "none",
                      rotation = "clock", zero = -55) 

plot.circular(windr, stack=T, bg="RoyalBlue", pch=21, cex=1.3,
              main = "Wind direction (Tower)", shrink = 1.5)

arrows.circular(mean(windr), col = "RoyalBlue")
  
  
#Wind Diretion (Darro)
setwd("C:/Users/Eduardo Q Marques/Documents/Research/Doutorado/Banco de Dados Tanguro/Area1-plot/Dados das torres")

darro = read.csv("Master_Estacao_Darro_2020.csv", sep = ",")

blowd = darro %>% 
  filter(Year == 2019 & Month == 2 & Day == 3) %>% 
  select(Date2, vel_vento_Max, windDir)

blowd2 = blowd[,3] %>% 
  na.omit()

windrd <- circular(blowd2, units = "degrees", template = "none",
                  rotation = "clock", zero = -55) 

plot.circular(windrd, stack=T, bg="RoyalBlue", pch=21, cex=1.3,
              main = "Wind direction (Darro)", shrink = 1.5)

arrows.circular(mean(windrd), col = "RoyalBlue")  


#All
par(mfrow=c(1,3))
plot.circular(tree_fall, stack=T, bg="RoyalBlue", pch=21, cex=1.5,
              main = "Trees falling direction", shrink = 1)
arrows.circular(mean(tree_fall), col = "RoyalBlue")
  
plot.circular(windr, stack=T, bg="RoyalBlue", pch=21, cex=1.5,
              main = "Wind direction (Tower)", shrink = 1)
arrows.circular(mean(windr), col = "RoyalBlue") 
  
plot.circular(windrd, stack=T, bg="RoyalBlue", pch=21, cex=1.5,
              main = "Wind direction (Darro)", shrink = 1)
arrows.circular(mean(windrd), col = "RoyalBlue")  

#Compare Tower and Darro stations ----------------------------------------------------------------------
blow$Station = c("Tower")
colnames(blow) = c("Date", "Wind_Speed", "Wind_Direction", "Station")
blowd$Station = c("Darro")
colnames(blowd) = c("Date", "Wind_Speed", "Wind_Direction", "Station")

stations = rbind(blow, blowd)

ggplot(stations, aes(x=Date, y=Wind_Speed, col = Station))+
  geom_line(aes(group = Station), size = 1)+
  theme_bw()+
  xlab(NULL)+
  ylab("Max Wind Speed")+
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position=c(.20,.75))








