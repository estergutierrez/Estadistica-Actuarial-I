library("readxl")
library("dplyr")
library("tidyverse")
library("cowplot")
library("xtable")


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

# Bases de datos por continente
df_Europa <- df_regions[df_regions$Region=="Europe",]
df_Asia <- df_regions[df_regions$Region=="Asia",]
df_America <-df_regions[df_regions$Region=="Americas", ]
df_Oceania <-df_regions[df_regions$Region=="Oceania", ]
df_Africa <-df_regions[df_regions$Region=="Africa", ]

#Resetear indices 
row.names(df_Africa) <- NULL
row.names(df_America) <- NULL
row.names(df_Asia)<- NULL
row.names(df_Europa)<- NULL
row.names(df_Oceania)<- NULL


### Graficos
# Dimensiones
#Dimensiones para todos los gráficos 
h<-5
w<-h*1.6

#Globales
ggplot(data=df_regions, mapping = aes(x =df_regions[,8], y=df_regions[,9]))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "EVN",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHesperanza.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =df_regions[,8], y=df_regions[,10]))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Desempleo total",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 23)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHDesempleo.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =df_regions[,8], y=df_regions[,11]))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "PNB per cápita",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHPNB.pdf",width = w,height = h)

ggplot(data=df_regions, mapping = aes(x =df_regions[,8], y=df_regions[,12]))+
  geom_point(colour="blue4", shape=16,size=1)+ 
  labs(x = "IDH",
       y = "Deflactor del PIB",
       title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))

ggsave(filename = "IDHDef.pdf",width = w,height = h)

 # Africa 
ggplot(data = df_Africa, aes(x = df_Africa[,8], y = df_Africa[,9])) +
  geom_point(color = 'red')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_Africa.pdf",width = w,height = h)


ggplot(data = df_Africa, aes(x = df_Africa[,8], y = df_Africa[,10])) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Africa.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Africa, aes(x = df_Africa[,8], y = df_Africa[,11])) +
  geom_point(color = 'red')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Africa.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Africa, aes(x = df_Africa[,8], y = df_Africa[,12])) +
  geom_point(color = 'red')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Africa.pdf",width = w,height = h)

# America 
ggplot(data = df_America, aes(x = df_America[,8], y = df_America[,9])) +
  geom_point(color = 'deeppink')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_America.pdf",width = w,height = h)


ggplot(data = df_America, aes(x = df_America[,8], y = df_America[,10])) +
  geom_point(color = 'deeppink')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_America.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_America, aes(x = df_America[,8], y = df_America[,11])) +
  geom_point(color = 'deeppink')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_America.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_America, aes(x = df_America[,8], y = df_America[,12])) +
  geom_point(color = 'deeppink')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_America.pdf",width = w,height = h)



# Asia
ggplot(data = df_Asia, aes(x = df_Asia[,8], y = df_Asia[,9])) +
  geom_point(color = 'deepskyblue2')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_Asia.pdf",width = w,height = h)


ggplot(data = df_Asia, aes(x = df_Asia[,8], y = df_Asia[,10])) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Asia.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Asia, aes(x = df_Asia[,8], y = df_Asia[,11])) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Asia.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Asia, aes(x = df_Asia[,8], y = df_Asia[,12])) +
  geom_point(color = 'deepskyblue2')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Asia.pdf",width = w,height = h)


# Europa
ggplot(data = df_Europa, aes(x = df_Europa[,8], y = df_Europa[,9])) +
  geom_point(color = 'limegreen')+
  labs(x = "IDH",y = "EVN",title=" ")+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHEVN_Europa.pdf",width = w,height = h)


ggplot(data = df_Europa, aes(x = df_Europa[,8], y = df_Europa[,10])) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "IDH", y ="Desempleo total"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDesempleo_Europa.pdf",width = w,height = h)

# IDH,  producto nacional bruto per capita
ggplot(data = df_Europa, aes(x = df_Europa[,8], y = df_Europa[,11])) +
  geom_point(color = 'limegreen')+
  labs(
    title = "", x = "IDH", y ="PNB Per Cápita"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHPNB_Europa.pdf",width = w,height = h)


# IDH, Deflactor del PIB
ggplot(data = df_Europa, aes(x = df_Europa[,8], y = df_Europa[,12])) +
  geom_point(color = 'limegreen')+
  labs(
    title = " ", x = "IDH", y ="Deflactor del PIB"
  )+ 
  theme_minimal_hgrid(font_size = 20)+
  theme(axis.title.y = element_text(size=rel(1), angle = 0, hjust = 1 ))
ggsave(filename = "IDHDeflactor_Europa.pdf",width = w,height = h)



