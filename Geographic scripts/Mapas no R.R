#Fazendo mapas no R

#By: Eduardo Queiroz Marques
#Instituto de Pesquisa Ambiental da Amazonia

#OBS: Precisa ter shapes e outros arquivos necessarios baixados

#1-Baixando e abrindo os pacotes####

#Baixar os seguintes pacotes
install.packages("raster")
install.packages("rgdal")
install.packages("maptools")
#Abrir os seguintes pacotes
library(raster)
library(rgdal)
library(maptools)

#2-Definindo o diretorio e ver os arquivos####
setwd("D:/Curso-R/R workshop dados espaciais") #buscar caminho da pasta
getwd() #ver o caminho do diretorio
list.files() #ver arquivos na pasta
list.files("Rasters ambientais") #ver arquivos na pasta Raster ambientais

#3-Lendo os arquivos raster####
#OBS: Quando um raster tem somente uma camada usamos a funÃ§Ã£o raster
temp.m=raster("Rasters ambientais/bio1_clip.tif")
temp.m #olhar resumo do raster

#4-Fazendo um plot do raster####
x11() #Plotar em janela separada (OBS:fica melhor)
plot(temp.m)

#5-Extraindo valores do raster como um vector####
temp.m.val=temp.m[] #extrai o valor para o vector
hist(temp.m.val) #histograma dos valores
min(na.omit(temp.m.val)) #remove o valor fora da area do vector (NA)
range(na.omit(temp.m.val)) #range da valor maximo e minimo do vector

#6-Lendo os outros arquivos####
temp.var=raster("Rasters ambientais/bio4_clip.tif")
precip.m=raster("Rasters ambientais/bio12_clip.tif")
precip.var=raster("Rasters ambientais/bio15_clip.tif")

#7-Combinando os rasters em um objeto de "stack" com 4 camadas####
#OBS: para isso Ã© necessario todos os atribuitos essenciais do arquivo estarem iguais(ex: resoluÃ§Ã£o, numero de pixels)
amb.stack=stack(temp.m,temp.var,precip.m,precip.var) #juntar os arquivos

#8-Mudando o nome das camadas####
names(amb.stack)=c("temp.m","temp.var","precip.m","precip.var")
amb.stack

#9-Aplicando funcoes aos rasters####
#OBS: isso sÃ£o apenas um exemplo
temp.m.dob=2*temp.m #multiplica todos valores por 2
amb.stack.sum=calc(amb.stack,fun=sum) #soma os valores
amb.stack.sum
plot(amb.stack.sum)
rm(temp.m.dob) #remove o arquivo de exmplo
rm(amb.stack.sum) #remove o arquivo de exmplo

#10-Fazendo Shapefile####
cerrado=readOGR(dsn = "Cerrado shape",layer="cerrado") #ler os arquivos de shape
cerrado #resumo do arquivo
plot(cerrado) #mostrar shape

#11-Montando mapas e mudando projeção####

#Fazer mapa de temperatura media do cerrado
plot(temp.m) #plota temperatura
lines(cerrado) #coloca os limites do cerrado

#Mudar projeção para os dados ficarem compativeis
proj=projection(temp.m) #salvar o sistema de projeção usada pelo temp.m(que é nossa projeção padrão)
cerrado.wgs=spTransform(cerrado,proj) #transforma o sistema de projeção do cerrado igual a do temp.m

#Extrair dados que ocorrem no cerrado
temp.m.mask=mask(temp.m,cerrado.wgs) #coloca uma mascara em cima dos dados fora do cerrado
plot(temp.m.mask)

#Mapa com todas as camadas
amb.stack.mask=mask(amb.stack,cerrado.wgs) #coloca mascara em cima dos dados juntos
plot(amb.stack.mask)

#Salvando os mapas
writeRaster(amb.stack.mask,file="temp_mak.tif") #salva nosso arquivo de imagem
savePlot("Mapa das variaveis do cerrado",type="jpg") #OBS: eu escolhi salvar em jpg

#12-Fazendo shapes####

#Fazer um shape com pontos de (x,y)
plots.dat=read.table("plots.txt",header = T) #Abrir a planilha de coordenadas
plots.shp=plots.dat #muda o nome dos arquivos
coordinates(plots.shp)<-~x+y #importa a coodenadas para o objeto
plots.shp #mostrar os dados do arquivo
projection(plots.shp)<-proj
plot(plots.shp) #mostrar nossos pontos
plots.shp@data #mostra os atributos dos plots

#Vamos fazer um mapa com nosso shape de pontos
plot(temp.m.mask) #plota a imagem já trabalhada
points(plots.shp) #plota os pontos na imagem

#Extrair as variaveis ambientais dos pontos que criamos
plots.amb=extract(amb.stack.mask,plots.shp)
plots.amb=as.data.frame(plots.amb)
plots.amb$plot=plots.shp@data$plot
plots.amb

#Salvar um shape
writeSpatialShape(plots.shp,"plots")