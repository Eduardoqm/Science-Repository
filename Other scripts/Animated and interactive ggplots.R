# libraries:
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)
library(plotly)

#Animated
ggplot(bio_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  transition_reveal(data)



ndvi$gradient = as.numeric(ndvi$gradient)

ggplot(ndvi, aes(gradient,ndvi, col=plots))+ 
  geom_line(aes(group=plots), size = 1)+
  labs(fill= "Plot",x="Distancia da borda (m)",y="NDVI")+
  theme_minimal()+
  transition_reveal(gradient)

# Save at gif:
setwd("C:/Users/Eduardo Q Marques/Documents")
anim_save("biochemistry_b1yr.gif")



#Interative
p <- ggplot(b3yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="line-mode1")
#chart_link