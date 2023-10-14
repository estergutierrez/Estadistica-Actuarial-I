library("readxl")
library("dplyr")
library("tidyverse")
library("cowplot")
library("data.table")
library("writexl")
library("xtable")
library("viridis")
library("data.table")


### Creacion de la base de datos
#Depuración de inflación
inflacion <- read_excel("C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/Deflactor.xlsx")

df_inflacion <-subset(inflacion, select = c(4,7))

df_inflacion <- df_inflacion %>% rename("Deflactor de PIB" = "Value")
df_inflacion <- df_inflacion[-c(218:258),]


# Cargar datos del banco mundial: Desempleo y GNI
desempleo <- read_excel("C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/Desempleo.xlsx")
GNI <- read_excel("C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/GNI.xlsx")

# Eliminar las filas innecesarias
df_desempleo <- desempleo[-c(218, 219,220,221,222),]
df_GNI <- GNI[-c(218, 219,220,221,222),]

# Juntarlas en una misma base de datos 
df2 <- merge(x=df_desempleo,y=df_GNI, 
             by=c("Country Name", "Country Code"), all=TRUE)

df2 <- df2 %>% rename("Desempleo total (% del total de la fuerza laboral)" = "2020 [YR2020].x", 
                      "Producto Nacional Bruto per capita" = "2020 [YR2020].y", "Country"="Country Name")

# Eliminar columnas inncesarias
df2 <- subset(df2, select = -c(3,4,6,7))


# Juntar la base de datos de inflación con la anterior 

df_temp <- merge(x=df2,y=df_inflacion, 
                 by=c("Country Code"), all=TRUE)
# Cambio de nombre de columnas
df_temp <- df_temp %>% rename("Code" = "Country Code")

# Eliminar columnas inncesarias
df_temp <- subset(df_temp, select = -c(2))


# Cargar las bases de datos restantes: IDH y esperanza de vida
IDH <-read.csv(file = "C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/human-development-index.csv")
Life <-read.csv(file = "C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/life-expectancy.csv")


# Filtrar columna Year
df_IDH <- IDH[IDH$Year==2020,]
df_Life <- Life[Life$Year==2020,]

# Resetear indices 
rownames(df_IDH) <- NULL
rownames(df_Life) <- NULL

# Eliminar las regiones
df_IDH <- df_IDH[!grepl("UNDP", df_IDH$Entity),]

#Resetear indices
rownames(df_IDH) <- NULL

# Intersecar los dataframes con left join
df3 <- merge(x=df_IDH,y=df_Life, 
             by=c("Code", "Year"), all.x=TRUE)


# Resetear indices
rownames(df3) <- NULL

# Cambiar nombre de columnas

df3 <- df3 %>% rename("Indice de desarrollo humano" = "Human.Development.Index", 
                      "Esperanza de vida al nacer" = "Life.expectancy.at.birth..historical.")

#Eliminar columnas inncesarias

df3 <- subset(df3, select = -c(2,3,5))


# Unir las bases de datos por codigo 
df_complete <- merge(x=df3,y=df_temp, 
                     by=c("Code"), all=TRUE)
# Cargar base de datos con los codigos ISO de 3 digitos (ISO 3166 international standard).
ISO <-read.csv(file = "C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/all.csv")
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/LICENSE.md


# Cambio de nombres de columnas
df_ISO <- ISO %>% rename("Pais" = "name", "Code" = "alpha.3")

# Juntar bases de datos con left join
df_regions <-merge(x=df_complete,y=df_ISO, by="Code", all.x=TRUE)

# Eliminar columnas inncesarias 
df_regions <- subset(df_regions, select = -c(8,10,13,16))

# ordenar las columnas
df_regions <- subset(df_regions, select = c(1,8,7,9,11,10,12,2,3,4,5,6))

# Encontrar los valores NA  
df_regions[is.na(df_regions$Pais), ]  

# Eliminar fila 151
df_regions <- df_regions[-c(151),] 
colnames(df_regions)

# Resetear indices
rownames(df_regions) <-NULL

# Agregar informacion faltante
df_regions[213,3] = "Kosovo"
df_regions[213,4] = "Europe"
df_regions[213,5] = 150

df_regions[35,3] = "Channel Islands"
df_regions[35,4] = "Europe"
df_regions[35,5] = 150

# Reemplazar puntos por NA
df_regions[df_regions == ".."] <- NA


#Cambio de nombres de las columnas
df_regions <- df_regions %>% rename("Codigo del Pais"="Code", "Codigo numerico del Pais"="country.code",
                                    "Region"="region", "Codigo númerico de la region"= "region.code",
                                    "Subregion"="sub.region", "Codigo númerico de la subregion" ="sub.region.code")
# Cambiar tipo de variable de las columnas
df_regions$`Desempleo total (% del total de la fuerza laboral)` <-as.numeric(as.character(df_regions$`Desempleo total (% del total de la fuerza laboral)`))
df_regions$`Producto Nacional Bruto per capita`<-as.numeric(as.character(df_regions$`Producto Nacional Bruto per capita`))
df_regions$`Deflactor de PIB`<-as.numeric(as.character(df_regions$`Deflactor de PIB`))




write.csv(df_regions, "C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/base_regiones_corregidas.csv", row.names=FALSE) 
#Eliminar Oceania
df_regions<-df_regions[df_regions$Region != "Oceania", ]

#Eliminar columnas innecesarias 
df_regions <- subset(df_regions, select = -c(6,7))




# Bases de datos por continente
df_Europa <- df_regions[df_regions$Region=="Europe",]
df_Asia <- df_regions[df_regions$Region=="Asia",]
df_America <-df_regions[df_regions$Region=="Americas", ]
df_Africa <-df_regions[df_regions$Region=="Africa", ]

#Resetear indices 
row.names(df_Africa) <- NULL
row.names(df_America) <- NULL
row.names(df_Asia)<- NULL
row.names(df_Europa)<- NULL


#Trasformacion 
df_ln<-df_regions
#Aplicar ln a los indicadores
df_ln[,c(6,7,8,9,10)] <- log(df_ln[,c(6,7,8,9,10)])


# Bases de datos por continente
df_Europaln <- df_ln[df_ln$Region=="Europe",]
df_Asialn <- df_ln[df_ln$Region=="Asia",]
df_Americaln <-df_ln[df_ln$Region=="Americas", ]
df_Africaln <-df_ln[df_ln$Region=="Africa", ]


#Resetear indices 
row.names(df_Africaln) <- NULL
row.names(df_Americaln) <- NULL
row.names(df_Asialn)<- NULL
row.names(df_Europaln)<- NULL


#Resetear indices 
row.names(df_Africa) <- NULL
row.names(df_America) <- NULL
row.names(df_Asia)<- NULL
row.names(df_Europa)<- NULL



### Graficos
# Dimensiones
#Dimensiones para todos los gráficos 
h<-5
w<-h*1.6

#Scatterplots
#Globales
ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`))+
  geom_point(colour="#4F6D7A", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "EVN",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHesperanza.pdf",width = w,height = h)


ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`))+
  geom_point(colour="#dd6e42", shape=16,size=1)+ 
  labs(x = "lnIDH",
       y = "lnEVN",
       title=" ")+expand_limits(x= 0)+ 
  theme_minimal_hgrid(font_size = 18)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHesperanza.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`))+
  geom_point(colour="#4F6D7A", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Desempleo total",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  ylab(expression(paste("Desempleo \ntotal" )))

ggsave(filename = "IDHDesempleo.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`))+
  geom_point(colour="#dd6e42", shape=16,size=1)+ 
  labs(x = "lnIDH",
       y = "lnDesempleo",
       title=" ")+ expand_limits(x= 0)+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "logIDHDesempleo.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`))+
  geom_point(colour="#4F6D7A", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "PNB per cápita",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  ylab(expression(paste("PNB per \ncápita" )))

ggsave(filename = "IDHPNB.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`))+
  geom_point(colour="#dd6e42", shape=16,size=1)+ 
  labs(x = "lnIDH",
       y = "lnPNB per cápita",
       title=" ")+ expand_limits(x= 0)+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  ylab(expression(paste("ln PNB per \ncápita" )))

ggsave(filename = "logIDHPNB.pdf",width = w,height = h)


ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`))+
  geom_point(colour="#4F6D7A", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Deflactor del PIB",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  ylab(expression(paste("Deflactor \ndel PIB" )))

ggsave(filename = "IDHDef.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`))+
  geom_point(colour="#dd6e42", shape=16,size=1)+ expand_limits(x= 0)+
  labs(x = "ln IDH",
       y = "ln Deflactor del PIB",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  ylab(expression(paste("ln Deflactor \ndel PIB" )))

ggsave(filename = "logIDHDef.pdf",width = w,height = h)







# Box plots
# IDH
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Indice de desarrollo humano` , fill=Region))+
  labs(x = "Continentes",y = "IDH", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))
ggsave(filename = "boxplot_IDH.pdf",width = w,height = h)

#IDH ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Indice de desarrollo humano` , fill=Region))+
  labs(x = "Continentes",y = "lnIDH", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))
ggsave(filename = "logboxplot_IDH.pdf",width = w,height = h)


# EVN 
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Esperanza de vida al nacer` , fill=Region))+
  labs(x = "Continentes",y = "EVN", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "boxplot_EVN.pdf",width = w,height = h)

# EVN ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Esperanza de vida al nacer` , fill=Region))+
  labs(x = "Continentes",y = "lnEVN", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "logboxplot_EVN.pdf",width = w,height = h)


# Desempleo
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Desempleo total (% del total de la fuerza laboral)` , fill=Region))+
  labs(x = "Continentes",y = "Desempleo", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))
ggsave(filename = "boxplot_Desempleo.pdf",width = w,height = h)

# Desempleo ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Desempleo total (% del total de la fuerza laboral)` , fill=Region))+
  labs(x = "Continentes",y = "lnDesempleo", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  theme(legend.position="none")+ scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))
ggsave(filename = "logboxplot_Desempleo.pdf",width = w,height = h)

# PNB 
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Producto Nacional Bruto per capita` , fill=Region))+
  labs(x = "Continentes",y = "PNB per cápita", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))+
  ylab(expression(paste("PNB per\ncápita" )))
ggsave(filename = "boxplot_PNB.pdf",width = w,height = h)

# PNB ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Producto Nacional Bruto per capita` , fill=Region))+
  labs(x = "Continentes",y = "lnPNB per cápita", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))+
  ylab(expression(paste("ln PNB per\ncápita" )))
ggsave(filename = "logboxplot_PNB.pdf",width = w,height = h)

# Deflactor
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Deflactor de PIB` , fill=Region))+
  labs(x = "Continentes",y = "PNB per cápita", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "boxplot_Deflactor.pdf",width = w,height = h)

ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Deflactor de PIB` , fill=Region))+
  labs(x = "Continentes",y = "ln Deflactor del PIB", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "logboxplot_Deflactor.pdf",width = w,height = h)


# Barplots
ggplot(data=df_regions, aes(x=`Region`, fill=Region)) +
  geom_bar()+
  labs(x = "Continentes",y = "Total de países", title=" ")+expand_limits(y=58)+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+ 
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+
  theme(legend.position="none")+scale_fill_manual(values=c("#DD6E42","#E8DAB2" , "#AEC5EB" ,"#98A88A"))+
  ylab(expression(paste("Total de \npaíses" )))
ggsave(filename = "barplot_regiones.pdf",width = w,height = h)

### Cuadros
# Cantidad total de datos
# Cantidad de observaciones por continente 
datos_total <-df_regions %>% count(Region)
datos_total <- datos_total %>% rename("Total de paises" = "n" )
xtable(datos_total)

# Vector de columna
Indicadores <-c("Indice de desarrollo humano", "Esperanza de vida al nacer","Desempleo total (% del total de la fuerza laboral)",
                "Producto Nacional Bruto per capita","Deflactor de PIB")
# America
datos_faltantesAM <-c(sum(is.na(df_America$`Indice de desarrollo humano`)), sum(is.na(df_America$`Esperanza de vida al nacer`)),
                      sum(is.na(df_America$`Desempleo total (% del total de la fuerza laboral)`)), sum(is.na(df_America$`Producto Nacional Bruto per capita`)),
                      sum(is.na(df_America$`Deflactor de PIB`)))
df_cantidadfaltaAmerica <- data.frame(Indicadores, datos_faltantesAM)
colnames(df_cantidadfaltaAmerica)

df_cantidadfaltaAmerica <- df_cantidadfaltaAmerica %>% rename("Cantidad de datos faltantes"="datos_faltantesAM")
xtable(df_cantidadfaltaAmerica, include.rownames = FALSE)



# Asia
datos_faltantesAS <-c(sum(is.na(df_Asia$`Indice de desarrollo humano`)), sum(is.na(df_Asia$`Esperanza de vida al nacer`)),
                      sum(is.na(df_Asia$`Desempleo total (% del total de la fuerza laboral)`)), sum(is.na(df_Asia$`Producto Nacional Bruto per capita`)),
                      sum(is.na(df_Asia$`Deflactor de PIB`)))
df_cantidadfaltaAsia <- data.frame(Indicadores, datos_faltantesAS)
df_cantidadfaltaAsia <- df_cantidadfaltaAsia %>% rename("Cantidad de datos faltantes"="datos_faltantesAS")
xtable(df_cantidadfaltaAsia, include.rownames = FALSE)

# Africa
datos_faltantesAF <-c(sum(is.na(df_Africa$`Indice de desarrollo humano`)), sum(is.na(df_Africa$`Esperanza de vida al nacer`)),
                      sum(is.na(df_Africa$`Desempleo total (% del total de la fuerza laboral)`)), sum(is.na(df_Africa$`Producto Nacional Bruto per capita`)),
                      sum(is.na(df_Africa$`Deflactor de PIB`)))
df_cantidadfaltaAfrica <- data.frame(Indicadores, datos_faltantesAF)
df_cantidadfaltaAfrica <- df_cantidadfaltaAfrica %>% rename("Cantidad de datos faltantes"="datos_faltantesAF")
xtable(df_cantidadfaltaAfrica, include.rownames = FALSE)

# Europa
datos_faltantesEU <-c(sum(is.na(df_Europa$`Indice de desarrollo humano`)), sum(is.na(df_Europa$`Esperanza de vida al nacer`)),
                      sum(is.na(df_Europa$`Desempleo total (% del total de la fuerza laboral)`)), sum(is.na(df_Europa$`Producto Nacional Bruto per capita`)),
                      sum(is.na(df_Europa$`Deflactor de PIB`)))
df_cantidadfaltaEuropa <- data.frame(Indicadores, datos_faltantesEU)
df_cantidadfaltaEuropa <- df_cantidadfaltaEuropa %>% rename("Cantidad de datos faltantes"="datos_faltantesEU")
xtable(df_cantidadfaltaEuropa, include.rownames = FALSE)



# Resumen de 5 numeros
df_summary <-subset(df_regions, select = c(4,8,9,10,11,12))
summary_dat <-
  df_summary %>%
  pivot_longer(c(`Indice de desarrollo humano`, `Esperanza de vida al nacer`,
                 `Desempleo total (% del total de la fuerza laboral)`,
                 `Producto Nacional Bruto per capita`, `Deflactor de PIB`), names_to = "Indicador") %>%group_by(Region, Indicador) %>% 
  summarize(minimo = fivenum(value)[1], Q1 = fivenum(value)[2], mediana = fivenum(value)[3], 
            Q3 = fivenum(value)[4], maximo = fivenum(value)[5])
df_summaryln <-subset(df_ln, select = c(4,8,9,10,11,12))
summary_datln <-
  df_summaryln %>%
  pivot_longer(c(`Indice de desarrollo humano`, `Esperanza de vida al nacer`,
                 `Desempleo total (% del total de la fuerza laboral)`,
                 `Producto Nacional Bruto per capita`, `Deflactor de PIB`), names_to = "Indicador") %>%group_by(Region, Indicador) %>% 
  summarize(minimo = fivenum(value)[1], Q1 = fivenum(value)[2], mediana = fivenum(value)[3], 
            Q3 = fivenum(value)[4], maximo = fivenum(value)[5])

# IDH
summary_IDH <-summary_dat[summary_dat$Indicador=="Indice de desarrollo humano",]
summary_IDHln <-summary_datln[summary_datln$Indicador=="Indice de desarrollo humano",]

df_summaryIDH <- merge(x=summary_IDH,y=summary_IDHln, 
                       by=c("Region", "Indicador"), all=TRUE)
df_summaryIDH<-t(df_summaryIDH)
xtable(df_summaryIDH)

#EVN
summary_EVN <-summary_dat[summary_dat$Indicador=="Esperanza de vida al nacer",]
summary_EVNln <-summary_datln[summary_datln$Indicador=="Esperanza de vida al nacer",]

df_summaryEVN <- merge(x=summary_EVN,y=summary_EVNln, 
                       by=c("Region", "Indicador"), all=TRUE)

df_summaryEVN[] <- lapply(df_summaryEVN, function(x) if(is.numeric(x)) round(x, 2) else x)
df_summaryEVN<-format(df_summaryEVN, decimal.mark=",")
df_summaryEVN <-t(df_summaryEVN)
xtable(df_summaryEVN)

#Deflactor
summary_Def <-summary_dat[summary_dat$Indicador=="Deflactor de PIB",]
summary_Defln <-summary_datln[summary_datln$Indicador=="Deflactor de PIB",]

df_summaryDef <- merge(x=summary_Def,y=summary_Defln, 
                       by=c("Region", "Indicador"), all=TRUE)

df_summaryDef[] <- lapply(df_summaryDef, function(x) if(is.numeric(x)) round(x, 2) else x)
df_summaryDef<-format(df_summaryDef, decimal.mark=",")
df_summaryDef <-t(df_summaryDef)
xtable(df_summaryDef)

