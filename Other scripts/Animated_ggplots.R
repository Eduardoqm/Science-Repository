# libraries:
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)

# Plot
ggplot(bio_b1yr, aes(data,index, col=indice))+ 
  geom_line(aes(group=indice), size = 1)+
  labs(fill= "Index",x="Ano",y="B3yr - Controle (% Relative difference)",
       title = "Biochemistry Indexes")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()+
  transition_reveal(data)

# Save at gif:
setwd("C:/Users/Eduardo Q Marques/Documents")
anim_save("biochemistry_b1yr.gif")
