## importa dados de produtividade do IBGE
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/master_corrigido")
dir()
pm5=read.csv("Produção_Agricola_corrigido_2002.csv")
pm5$cultura2=gsub(" ","",as.character(pm5$cultura))
pm5$estado=as.character(pm5$estado)

##importa nomes dos municipios
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/nomes_municipios")
dir()
est=read.csv("estados.csv")
est$estado=as.character(est$siglami) #Usado para fazer comparação dos estados
head(est)
##
##
pm05=merge(pm5,est[,c("estado","siglama")],by="estado",all.x=TRUE,all.y=FALSE)
pm05$id=paste(pm05$siglama,standardize(pm05$municipio),sep="_")
head(pm05)
str(pm05)

soja_ibge=subset(pm05,cultura2=="soja(emgrao)",select=c(id,siglama,area.plantada_ha))
head(soja_ibge)
names(soja_ibge)=c("id","est","sojaap05","sojaac05","sojap05","sojapr05")
soja_ibge2.2=subset(soja_ibge,est=="GO"|est=="MT"|est=="MA"|est=="PI"|
                  est=="BA"|est=="TO"|est=="DF")
head(soja05.2)