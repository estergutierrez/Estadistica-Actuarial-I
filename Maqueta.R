# Assumptions

# 1. Las variables mantienen una relación lineal
# 2. Las variables son independientes
# 3. Bivariate normal distribution
# 4. Constant variance 



# Ejemplo con Africa

# Se eliminan los países para los cuales falte uno o más datos

df_AfricaMaqueta <- na.omit(df_Africaln)
row.names(df_AfricaMaqueta) <- NULL

# Guardamos la muestra de cada uno de los indices en vectores

IDH <- df_AfricaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida <- df_AfricaMaqueta$`Esperanza de vida al nacer`
Desempleo <- df_AfricaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita <- df_AfricaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB <- df_AfricaMaqueta$`Deflactor de PIB`

# Las variables vienen en pares
length(IDH) == length(EsperanzaDeVida)
length(IDH) == length(Desempleo)
length(IDH) == length(PNBperCapita)
length(IDH) == length(DeflactorPIB)

# Ahora se va a verificar que los datos cumplen con los supuestos del coeficiente

# 1. En el analisis exploratorio de los datos se descubrió que algunas variables mantienen una relación no lineal
# Por lo tanto se aplicará logaritmo natural a todas las variables para eliminar el efecto de la escala y obtener 
# variables con relaciones lineales. Para esto observe los datos antes de la transformación y luego de la transformación,
# donde se puede apreciar la linealidad de la relación de las variables

# 2. Las variables son independientes: Se va a asumir la hipotésis y en caso contrario se rechaza, es decir
# que se establecerá una relación entre variables

# 3. Distribución normal

# http://www.sthda.com/english/wiki/normality-test-in-r
# https://pubmed.ncbi.nlm.nih.gov/22563845/ <- rho de Pearson robusta
# https://pubmed.ncbi.nlm.nih.gov/29795841/ <- Desempeño de Pearson respecto a otros valores
# alpha = 0.05 por default

shapiro.test(log(IDH))$p.value > 0.03
shapiro.test(log(EsperanzaDeVida))$p.value > 0.03
shapiro.test(log(Desempleo))$p.value > 0.03
shapiro.test(log(PNBperCapita))$p.value > 0.03
shapiro.test(log(DeflactorPIB))$p.value > 0.03

# Ahora se aplica el analisis de correlación de Pearson

corrIDH_EV <- cor(IDH, EsperanzaDeVida, method = 'pearson')
corrIDH_Desempleo <- cor(IDH, Desempleo, method = 'pearson')
corrIDH_PNB <- cor(IDH, PNBperCapita, method = 'pearson')
corrIDH_Inf <- cor(IDH[-c(38)], DeflactorPIB[-c(38)], method = 'pearson')

corrIDH_EV
corrIDH_Desempleo
corrIDH_PNB
corrIDH_Inf

Pearson <- c(corrIDH_EV, corrIDH_Desempleo, corrIDH_PNB, corrIDH_Inf)

# Correlación de Spearman
corrS_IDH_EV <- cor(IDH, EsperanzaDeVida, method = 'spearman')
corrS_IDH_Desempleo <- cor(IDH, Desempleo, method = 'spearman')
corrS_IDH_PNB <- cor(IDH, PNBperCapita, method = 'spearman')
corrS_IDH_Inf <- cor(IDH, DeflactorPIB, method = 'spearman')

corrS_IDH_EV
corrS_IDH_Desempleo
corrS_IDH_PNB
corrS_IDH_Inf

Spearman <- c(corrS_IDH_EV, corrS_IDH_Desempleo, corrS_IDH_PNB, corrS_IDH_Inf)
Variables <- c('IDH-EVN', 'IDH-Desempleo', 'IDH-PNB per cápita', 'IDH-Deflactor del PIB')

coeficientes <- data.frame(Variables, Pearson, Spearman)


ggplot(df_Africa, aes(x=`Deflactor de PIB`)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

ggplot(df_AfricaMaqueta, aes(x = `Deflactor de PIB`)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
