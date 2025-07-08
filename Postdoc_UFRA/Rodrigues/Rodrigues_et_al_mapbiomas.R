#ANALISE UTILIZANDO OS DADOS DO MAPBIOMAS

#RONER RODRIGUES 07-07-2025

library(sf) #importa e manipula shapes
library(terra) #importa e manipula rasters e shapes

#Carregando os dados ----------------------------------------------------------------------------------------------------
setwd("C:/Users/Eduardo/Documents/Programing/Science-Repository/Postdoc_UFRA/Rodrigues/dados") #CHAMA A PASTA COM OS DADOS
dir() #Mostra os dados dentro da pasta

cult = sf::st_read("Culturas permanentes_MicroGuama.shp") #shape das culturas permanentes
car = sf::st_read("CAR_nordeste_Para.shp") #shape do CAR dos municipios da micro
mb = rast("MB2023_l1_30m.tiff") #raster mapbiomas 2023

#plotando dados----------------------------------------------------------------------------------------------------------
plot(mb) #plotando dados do mapbiomas 2023
plot(cult[2], add=TRUE)  #plotando culturas permanentes (o numero 2 serve para especificar a coluna 2 da tabela)
plot(car[1], add=TRUE) #plotando o car (add=TRUE serve para plotar um plot sobre o outro)

#recortar raster --------------------------------------------------------------------------------------------------------
mb2 = crop(mb, car) # (recorta o raster pelo shape do car)
mb3 = mask(mb2, car) # ( serve para cortar o raster dentro do limite do shape)
plot(mb3)
plot(car[1], add=TRUE)

#Transformar shape do culturas permanentes em um raster com as 3 culturas -----------------------------------------------

#Colocar numero das calsses diferentes do MapBiomas (citrus-30, dende -31, pimenta - 32)


#Juntando o Mapbiomas e Culturas permantentes ---------------------------------------------------------------------------



#Extraindo dados de area antropizada e preservada -----------------------------------------------------------------------

#Aqui vc vai usar o shape do CAR para extrair dentro de cada propriedade a quantidade de pixels de cada classe