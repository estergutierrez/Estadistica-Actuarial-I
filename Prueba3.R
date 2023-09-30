library("readxl")
library("dplyr")
library("tidyverse")
library("cowplot")
library("data.table")
library("xtable")
library("viridis")


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

df_ln <-df_regions
df_ln[,c(8,9,10,11,12)] <- log(df_ln[,c(8,9,10,11,12)])


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
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "EVN",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHesperanza.pdf",width = w,height = h)


ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "logIDH",
       y = "logEVN",
       title=" ")+expand_limits(x= 0)+ 
  theme_minimal_hgrid(font_size = 18)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHesperanza.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Desempleo total",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHDesempleo.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "logIDH",
       y = "logDesempleo",
       title=" ")+ expand_limits(x= 0)+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "logIDHDesempleo.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "PNB per cápita",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHPNB.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "lnIDH",
       y = "lnPNB per cápita",
       title=" ")+ expand_limits(x= 0)+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "logIDHPNB.pdf",width = w,height = h)


ggplot(data=df_regions, mapping = aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Deflactor del PIB",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHDef.pdf",width = w,height = h)

ggplot(data=df_ln, mapping = aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`))+
  geom_point(colour="blue4", shape=16,size=1)+ expand_limits(x= 0)+
  labs(x = "ln IDH",
       y = "ln Deflactor del PIB",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "lnIDHDef.pdf",width = w,height = h)


 # Africa 
ggplot(data = df_Africa, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'red')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))


ggsave(filename = "IDHEVN_Africa.pdf",width = w,height = h)


ggplot(data = df_Africaln, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'red')+expand_limits(x= -0.2)+
  labs(x = "lnIDH",y = "lnEVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))


ggsave(filename = "logIDHEVN_Africa.pdf",width = w,height = h)


ggplot(data = df_Africa, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "IDH", y ="Desempleo"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Africa.pdf",width = w,height = h)

ggplot(data = df_Africaln, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "lnIDH", y ="lnDesempleo"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDesempleo_Africa.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Africa, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Africa.pdf",width = w,height = h)

ggplot(data = df_Africaln, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "lnIDH", y ="lnPNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHPNB_Africa.pdf",width = w,height = h)




# IDH, Deflactor del PIB
ggplot(data = df_Africa, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'red')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Africa.pdf",width = w,height = h)

ggplot(data = df_Africaln, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'red')+
  labs(
    title = " ", x = "lnIDH", y ="lnDeflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDeflactor_Africa.pdf",width = w,height = h)




# America 
ggplot(data = df_America, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'deeppink')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_America.pdf",width = w,height = h)


ggplot(data = df_America, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'deeppink')+
  labs(
    title = "", x = "IDH", y ="Desempleo"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_America.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_America, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'deeppink')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_America.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_America, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'deeppink')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_America.pdf",width = w,height = h)



# Asia
ggplot(data = df_Asia, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'deepskyblue2')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_Asia.pdf",width = w,height = h)


ggplot(data = df_Asia, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Asia.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Asia, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Asia.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Asia, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Asia.pdf",width = w,height = h)

# ln
ggplot(data = df_Asialn, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'deepskyblue2')+
  labs(x = "lnIDH",y = "lnEVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHEVN_Asia.pdf",width = w,height = h)


ggplot(data = df_Asialn, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "lnIDH", y ="lnDesempleo"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDesempleo_Asia.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Asialn, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "lnIDH", y ="lnPNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHPNB_Asia.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Asialn, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = " ", x = "lnIDH", y ="lnDeflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDeflactor_Asia.pdf",width = w,height = h)



# Europa
ggplot(data = df_Europa, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
  geom_point(color = 'limegreen')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_Europa.pdf",width = w,height = h)


ggplot(data = df_Europa, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Europa.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Europa, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Europa.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Europa, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Europa.pdf",width = w,height = h)


#ln 
ggplot(data = df_Europaln, aes(x =`Indice de desarrollo humano`, y=`Esperanza de vida al nacer`)) +
geom_point(color = 'limegreen')+
  labs(x = "lnIDH",y = "lnEVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHEVN_Europa.pdf",width = w,height = h)


ggplot(data = df_Europaln, aes(x =`Indice de desarrollo humano`, y=`Desempleo total (% del total de la fuerza laboral)`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "lnIDH", y ="lnDesempleo"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDesempleo_Europa.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Europaln, aes(x =`Indice de desarrollo humano`, y=`Producto Nacional Bruto per capita`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "lnIDH", y ="lnPNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHPNB_Europa.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Europaln, aes(x =`Indice de desarrollo humano`, y= `Deflactor de PIB`)) +
  geom_point(color = 'limegreen')+
  labs(
    title = " ", x = "lnIDH", y ="lnDeflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "logIDHDeflactor_Europa.pdf",width = w,height = h)







# Box plots
# IDH
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Indice de desarrollo humano` , fill=Region))+
  labs(x = "Continentes",y = "IDH", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "boxplot_IDH.pdf",width = w,height = h)

#IDH ln
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Indice de desarrollo humano` , fill=Region))+
  labs(x = "Continentes",y = "lnIDH", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
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
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "boxplot_Desempleo.pdf",width = w,height = h)

# Desempleo ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Desempleo total (% del total de la fuerza laboral)` , fill=Region))+
  labs(x = "Continentes",y = "lnDesempleo", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "logboxplot_Desempleo.pdf",width = w,height = h)

# PNB 
ggplot(data=df_regions)+
  geom_boxplot(mapping = aes(x = Region, y=`Producto Nacional Bruto per capita` , fill=Region))+
  labs(x = "Continentes",y = "PNB per cápita", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
ggsave(filename = "boxplot_PNB.pdf",width = w,height = h)

# PNB ln
ggplot(data=df_ln)+
  geom_boxplot(mapping = aes(x = Region, y=`Producto Nacional Bruto per capita` , fill=Region))+
  labs(x = "Continentes",y = "lnPNB per cápita", title=" ")+
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))+
  scale_x_discrete( labels = c("Africa" = "África","Americas" = "América", "Asia" = "Asia","Europe" = "Europa"))+ 
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, begin = 0.5)
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
  theme(legend.position="none")+ scale_fill_viridis(discrete = TRUE, option = "C", begin = 0.3)
ggsave(filename = "barplot_regiones.pdf",width = w,height = h)
df_bar <-df_regions
df_bar$IDH <- with(df_bar, ifelse(is.na(df_bar$`Indice de desarrollo humano`)
                                  , NA, "IDH"))
df_bar$EVN <- with(df_bar, ifelse(is.na(df_bar$`Esperanza de vida al nacer`)
                                  , NA, "EVN"))
df_bar$Des <- with(df_bar, ifelse(is.na(df_bar$`Desempleo total (% del total de la fuerza laboral)`)
                                  , NA, "Des"))
df_bar$PNB <- with(df_bar, ifelse(is.na(df_bar$`Producto Nacional Bruto per capita`)
                                  , NA, "PNB"))
df_bar$Deflactor <- with(df_bar, ifelse(is.na(df_bar$`Deflactor de PIB`)
                                  , NA, "DefPIB"))
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
# Vector columna
resumen <-c("Minimo", "Primer cuartil","Mediana", "Tercer cuartil", "Maximo")

resumen_global <-sapply(df_regions[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                    "Desempleo total (% del total de la fuerza laboral)",
                                    "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_global)<-resumen
resumen_global<-t(resumen_global)

resumen_globalln <-sapply(df_ln[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                     "Desempleo total (% del total de la fuerza laboral)",
                                     "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_globalln)<-resumen
resumen_globalln<-t(resumen_globalln)
# Africa
resumen_Africa <-sapply(df_Africa[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                   "Desempleo total (% del total de la fuerza laboral)",
                   "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
df_summaryAfrica <-data.frame(resumen,resumen_Africa)

row.names(resumen_Africa)<-resumen
resumen_Africa <- t(resumen_Africa)
xtable(resumen_Africa)
resumen_Africaln <-sapply(df_Africaln[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                    "Desempleo total (% del total de la fuerza laboral)",
                                    "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Africaln)<-resumen
resumen_Africaln <- t(resumen_Africaln)


# America
resumen_America <-sapply(df_America[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                "Desempleo total (% del total de la fuerza laboral)",
                                "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_America)<-resumen
resumen_America <- t(resumen_America)
resumen_Americaln <-sapply(df_Americaln[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                    "Desempleo total (% del total de la fuerza laboral)",
                                    "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Americaln)<-resumen
resumen_Americaln <- t(resumen_Americaln)



# Asia
resumen_Asia <-sapply(df_Asia[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                    "Desempleo total (% del total de la fuerza laboral)",
                                    "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Asia)<-resumen
resumen_Asia <- t(resumen_Asia)
resumen_Asialn <-sapply(df_Asialn[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                        "Desempleo total (% del total de la fuerza laboral)",
                                        "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Asialn)<-resumen
resumen_Asialn <- t(resumen_Asialn)


# Europa
resumen_Europa <-sapply(df_Europa[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                    "Desempleo total (% del total de la fuerza laboral)",
                                    "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Europa)<-resumen
resumen_Europa <- t(resumen_Europa)
resumen_Europaln <-sapply(df_Europaln[c("Indice de desarrollo humano", "Esperanza de vida al nacer",
                                        "Desempleo total (% del total de la fuerza laboral)",
                                        "Producto Nacional Bruto per capita","Deflactor de PIB")], fivenum)
row.names(resumen_Europaln)<-resumen
resumen_Europaln <- t(resumen_Europaln)
