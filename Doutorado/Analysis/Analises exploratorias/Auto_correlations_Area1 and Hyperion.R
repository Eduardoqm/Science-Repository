library(GGally)

struc = df %>% 
  select('dist','evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')

ggpairs(struc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel'))


bioc = df %>% 
  select('dist','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggpairs(bioc, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel'))


phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')

ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))



phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')


ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))

ggpairs(phy, aes(color = dist), axisLabels = "none", density =  ,
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))




ggduo(phy, aes(color = dist), types = list(continuous = "smooth"), axisLabels = "none")

ggduo(phy, aes(color = dist), axisLabels = "none")

ggcorr(phy)









