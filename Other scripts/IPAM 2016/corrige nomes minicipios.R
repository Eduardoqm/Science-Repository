library(ipam)

setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/master2")
file=dir()
csvFile=read.csv(file[1],stringsAsFactors=FALSE,encoding="UTF-8")
head(csvFile)
csvFile$municipio1=standardize(csvFile$municipio)

csvFile$municipio2=ifelse(csvFile$municipio1=="barro preto",
                         "governador lomanto junior",csvFile$municipio1)
unique(csvFile$municipio2=="barro preto")


## corrige nome de Itanhangá e ipiranga do norte
csvFile$municipio2=ifelse(csvFile$municipio2=="itanhanga!"|csvFile$municipio2=="itanhanga",
                          "tapurah",csvFile$municipio2)
csvFile$municipio2=ifelse(csvFile$municipio2=="ipiranga do norte","tapurah",csvFile$municipio2)
csvFile$municipio2=ifelse(csvFile$municipio2=="aroeiras do itaim","picos",csvFile$municipio2)

# teste para ver se deu certo
unique(csvFile$municipio2=="itanhanga")
unique(csvFile$municipio2=="itanhanga!")
unique(csvFile$municipio2=="ipiranga do norte")
unique(csvFile$municipio2=="aroeiras do itaim")


##

sumfun <- function(x, ...){
  c(s=sum(x, ...), l=length(x))
}
###
csvFile$area.plantada_ha=as.numeric(as.character(csvFile$area.plantada_ha))
csvFile$area.colhida_ha=as.numeric(as.character(csvFile$area.colhida_ha))
csvFile$producao_t=as.numeric(as.character(csvFile$producao_t))
csvFile$produtividade_kg_ha=as.numeric(as.character(csvFile$produtividade_kg_ha))
csvFile$valor_1000rs=as.numeric(as.character(csvFile$valor_1000rs))

df=na.omit(csvFile)
dim(csvFile);dim(df)


#### culturas que precisam ser corrigidas
csvFile$cultura2=gsub(" ","",standardize(csvFile$cultura))
ct=sort(unique(csvFile$cultura2));ct


####
head(csvFile)
str(csvFile)
dff=doBy::summaryBy(area.plantada_ha+area.colhida_ha+
                producao_t+valor_1000rs+produtividade_kg_ha~estado+municipio2+ano+cultura2,
                data=csvFile,FUN=sumfun,na.rm=TRUE,keep.names=TRUE)
###
###
head(dff)
dff$produtividade_kg_ha=dff$produtividade_kg_ha.s/dff$produtividade_kg_ha.l

###
dff2=subset(dff,select=c(estado,municipio2,ano,cultura2,area.plantada_ha.s,
                         area.colhida_ha.s,producao_t.s,produtividade_kg_ha,valor_1000rs.s))

names(dff2)=c("estado","municipio","ano","cultura","area.plantada_ha",
            "area.colhida_ha","producao_t","produtividade_kg","valor_1000rs")
head(dff2)


###
setwd("/mnt/data/dados_publicos/Documents/producao_agricola_municipal_2002_2014/master2")
write.csv(dff2,"Produção_Agricola_corrigido_2002_DVS.csv")
