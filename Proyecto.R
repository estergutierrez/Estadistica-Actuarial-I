######################################################################## Africa

# Se eliminan los países para los cuales falte uno o más datos

df_AfricaMaqueta <- na.omit(df_Africaln)
row.names(df_AfricaMaqueta) <- NULL

# Guardamos la muestra de cada uno de los indices en vectores

IDH_Africa <- df_AfricaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Africa <- df_AfricaMaqueta$`Esperanza de vida al nacer`
Desempleo_Africa <- df_AfricaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Africa <- df_AfricaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Africa <- df_AfricaMaqueta$`Deflactor de PIB`

# Ahora se va a verificar que los datos cumplen con los supuestos del coeficiente

# 1. En el analisis exploratorio de los datos se descubrió que algunas variables mantienen una relación no lineal
# Por lo tanto se aplicará logaritmo natural a todas las variables para eliminar el efecto de la escala y obtener 
# variables con relaciones lineales. Para esto observe los datos antes de la transformación y luego de la transformación,
# donde se puede apreciar la linealidad de la relación de las variables

# 2. Las variables son independientes: Se va a asumir la hipotésis y en caso contrario se rechaza, es decir
# que se establecerá una relación entre variables

# 3. Distribución normal

shapiro.test(IDH_Africa)
shapiro.test(EsperanzaDeVida_Africa)
shapiro.test(Desempleo_Africa)
shapiro.test(PNBperCapita_Africa)
shapiro.test(DeflactorPIB_Africa)


# Ahora se aplica el analisis de correlación de Pearson

# Función en R

corrIDH_EV <- cor(IDH_Africa, EsperanzaDeVida_Africa, method = 'pearson')
corrIDH_Desempleo <- cor(IDH_Africa, Desempleo_Africa, method = 'pearson')
corrIDH_PNB <- cor(IDH_Africa, PNBperCapita_Africa, method = 'pearson')
corrIDH_Inf <- cor(IDH_Africa[-c(38)], DeflactorPIB_Africa[-c(38)], method = 'pearson')

Pearson_Africa <- c(corrIDH_EV, corrIDH_Desempleo, corrIDH_PNB, corrIDH_Inf)

# Correlación de Spearman
corrS_IDH_EV <- cor(IDH_Africa, EsperanzaDeVida_Africa, method = 'spearman')
corrS_IDH_Desempleo <- cor(IDH_Africa, Desempleo_Africa, method = 'spearman')
corrS_IDH_PNB <- cor(IDH_Africa, PNBperCapita_Africa, method = 'spearman')
corrS_IDH_Inf <- cor(IDH_Africa, DeflactorPIB_Africa, method = 'spearman')

Spearman_Africa <- c(corrS_IDH_EV, corrS_IDH_Desempleo, corrS_IDH_PNB, corrS_IDH_Inf)
Variables <- c('IDH-EVN', 'IDH-Desempleo', 'IDH-PNB per cápita', 'IDH-Deflactor del PIB')

coeficientes_Africa <- data.frame(Variables, Pearson_Africa, Spearman_Africa)

########################################################################### Asia

# Parece que no hay valores anómalos

df_AsiaMaqueta <- na.omit(df_Asialn)
row.names(df_AsiaMaqueta) <- NULL

IDH_Asia <- df_AsiaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Asia <- df_AsiaMaqueta$`Esperanza de vida al nacer`
Desempleo_Asia <- df_AsiaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Asia <- df_AsiaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Asia <- df_AsiaMaqueta$`Deflactor de PIB`

# Verificación de hipótesis

shapiro.test(IDH_Asia)
shapiro.test(EsperanzaDeVida_Asia)
shapiro.test(Desempleo_Asia)
shapiro.test(PNBperCapita_Asia)
shapiro.test(DeflactorPIB_Asia)

# Los datos de desempleo y deflactor del PIB no tienen distribución normal

# Ahora se aplica el analisis de correlación de Pearson

# Función en R

corrIDH_EV <- cor(IDH_Asia, EsperanzaDeVida_Asia, method = 'pearson')
corrIDH_Desempleo <- cor(IDH_Asia, Desempleo_Asia, method = 'pearson')
corrIDH_PNB <- cor(IDH_Asia, PNBperCapita_Asia, method = 'pearson')
corrIDH_Inf <- cor(IDH_Asia, DeflactorPIB_Asia, method = 'pearson')

Pearson_Asia <- c(corrIDH_EV, corrIDH_Desempleo, corrIDH_PNB, corrIDH_Inf)

# Correlación de Spearman
corrS_IDH_EV <- cor(IDH_Asia, EsperanzaDeVida_Asia, method = 'spearman')
corrS_IDH_Desempleo <- cor(IDH_Asia, Desempleo_Asia, method = 'spearman')
corrS_IDH_PNB <- cor(IDH_Asia, PNBperCapita_Asia, method = 'spearman')
corrS_IDH_Inf <- cor(IDH_Asia, DeflactorPIB_Asia, method = 'spearman')

Spearman_Asia <- c(corrS_IDH_EV, corrS_IDH_Desempleo, corrS_IDH_PNB, corrS_IDH_Inf)

coeficientes_Asia <- data.frame(Variables, Pearson_Asia, Spearman_Asia)

######################################################################### America


df_AmericaMaqueta <- na.omit(df_Americaln)
row.names(df_AmericaMaqueta) <- NULL

IDH_America <- df_AmericaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_America <- df_AmericaMaqueta$`Esperanza de vida al nacer`
Desempleo_America <- df_AmericaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_America <- df_AmericaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_America <- df_AmericaMaqueta$`Deflactor de PIB`

# Argentina es un valor anomalo (c[-1]) en inflación
# Haiti es un valor anomalo en el IDH [c(-16)]

# Verificación de hipótesis

shapiro.test(IDH_America[c(-16)])
shapiro.test(EsperanzaDeVida_America[c(-16)])
shapiro.test(Desempleo_America[c(-16)])
shapiro.test(PNBperCapita_America[c(-16)])
shapiro.test(DeflactorPIB_America[c(-1,-16)]) 

#El unico indicador sin distribución normal es el deflactor del PIB

# Ahora se aplica el analisis de correlación de Pearson

# Función en R

corrIDH_EV <- cor(IDH_America[c(-16)], EsperanzaDeVida_America[c(-16)], method = 'pearson')
corrIDH_Desempleo <- cor(IDH_America[c(-16)], Desempleo_America[c(-16)], method = 'pearson')
corrIDH_PNB <- cor(IDH_America[c(-16)], PNBperCapita_America[c(-16)], method = 'pearson')
corrIDH_Inf <- cor(IDH_America[c(-1,-16)], DeflactorPIB_America[c(-1,-16)], method = 'pearson')

Pearson_America <- c(corrIDH_EV, corrIDH_Desempleo, corrIDH_PNB, corrIDH_Inf)

# Correlación de Spearman
corrS_IDH_EV <- cor(IDH_America, EsperanzaDeVida_America, method = 'spearman')
corrS_IDH_Desempleo <- cor(IDH_America, Desempleo_America, method = 'spearman')
corrS_IDH_PNB <- cor(IDH_America, PNBperCapita_America, method = 'spearman')
corrS_IDH_Inf <- cor(IDH_America, DeflactorPIB_America, method = 'spearman')

Spearman_America <- c(corrS_IDH_EV, corrS_IDH_Desempleo, corrS_IDH_PNB, corrS_IDH_Inf)

coeficientes_America <- data.frame(Variables, Pearson_America, Spearman_America)

######################################################################### Europa

df_EuropaMaqueta <- na.omit(df_Europaln)
row.names(df_EuropaMaqueta) <- NULL

IDH_Europa <- df_EuropaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Europa <- df_EuropaMaqueta$`Esperanza de vida al nacer`
Desempleo_Europa <- df_EuropaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Europa <- df_EuropaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Europa <- df_EuropaMaqueta$`Deflactor de PIB`
# No hay valores anomalos
# Verificación de hipótesis

shapiro.test(IDH_Europa)
shapiro.test(EsperanzaDeVida_Europa)
shapiro.test(Desempleo_Europa)
shapiro.test(PNBperCapita_Europa)
shapiro.test(DeflactorPIB_Europa) 

# Solamente el desempleo y el PNB per capita tienen distribución normal

# Ahora se aplica el analisis de correlación de Pearson

# Función en R

corrIDH_EV <- cor(IDH_Europa, EsperanzaDeVida_Europa, method = 'pearson')
corrIDH_Desempleo <- cor(IDH_Europa, Desempleo_Europa, method = 'pearson')
corrIDH_PNB <- cor(IDH_Europa, PNBperCapita_Europa, method = 'pearson')
corrIDH_Inf <- cor(IDH_Europa, DeflactorPIB_Europa, method = 'pearson')

Pearson_Europa <- c(corrIDH_EV, corrIDH_Desempleo, corrIDH_PNB, corrIDH_Inf)

# Correlación de Spearman
corrS_IDH_EV <- cor(IDH_Europa, EsperanzaDeVida_Europa, method = 'spearman')
corrS_IDH_Desempleo <- cor(IDH_Europa, Desempleo_Europa, method = 'spearman')
corrS_IDH_PNB <- cor(IDH_Europa, PNBperCapita_Europa, method = 'spearman')
corrS_IDH_Inf <- cor(IDH_Europa, DeflactorPIB_Europa, method = 'spearman')

Spearman_Europa <- c(corrS_IDH_EV, corrS_IDH_Desempleo, corrS_IDH_PNB, corrS_IDH_Inf)

coeficientes_Europa <- data.frame(Variables, Pearson_Europa, Spearman_Europa)

coef <-  cbind(coeficientes_Africa, coeficientes_America,coeficientes_Asia, coeficientes_Europa)
coef <- subset(coef, select = c(-4,-7,-10))

