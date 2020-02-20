library(GGally)

struc = df %>% 
  select('data','parcela','evi','ndvi','nbri','vari','vig','biomass','lai','litter','fuel')

ggpairs(struc)


bioc = df %>% 
  select('data','parcela','ari','lwvi2','msi','ndii','pssr','psri','sipi','wbi','biomass','lai','litter','fuel')

ggpairs(bioc)


phy = df %>% 
  select('data','parcela','pri','rendvi','biomass','lai','litter','fuel')

ggpairs(phy)



phy = df %>% 
  select('dist','pri','rendvi','biomass','lai','litter','fuel')


ggpairs(phy, aes(color = dist), lower = list(continuous = "smooth"), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))

ggpairs(phy, aes(color = dist), axisLabels = "none",
        columns = c('pri','rendvi','biomass','lai','litter','fuel'))




ggduo(phy, aes(color = dist), types = list(continuous = "smooth"), axisLabels = "none")

ggduo(phy, aes(color = dist), axisLabels = "none")

ggcorr(phy)









