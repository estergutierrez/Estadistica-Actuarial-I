# Assumptions
# 1. Variables are continuous [Verified]
# 2. Variables are paired [Verified]
# 3. Variables are independent
# 4. There is a linear relationship between variables
# 5. Bivariate normal distribution
# 6. Homoscedasticity exists 
# 7. There should be no univariate or multivariate outliers.

# Source: https://statistics.laerd.com/statistical-guides/pearson-correlation-coefficient-statistical-guide.php

IDH <- df_Africa$`Indice de desarrollo humano`
EsperanzaDeVida <- df_Africa$`Esperanza de vida al nacer`
Desempleo <- df_Africa$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita <- df_Africa$`Producto Nacional Bruto per capita`
DeflactorPIB <- df_Africa$`Deflactor de PIB`

# 2. Las variables vienen en pares
length(IDH) == length(EsperanzaDeVida)
length(IDH) == length(Desempleo)
length(IDH) == length(PNBperCapita)
length(IDH) == length(DeflactorPIB)

# 3. Las variables son independientes
install.packages("lmtest")
library('lmtest')

model <- lm(IDH ~ EsperanzaDeVida)
dwtest(model)
# https://www.marsja.se/durbin-watson-test-in-r-step-by-step-incl-interpretation/
# Aplicar ln 

model <- lm(log(IDH) ~ log(EsperanzaDeVida))
dwtest(model)

# 4. Linealidad

plot(IDH ~ EsperanzaDeVida)

# 5. Distribución normal

# http://www.sthda.com/english/wiki/normality-test-in-r
# Esto y los gráficos demuestran la distribución normal
shapiro.test(log(IDH))$p.value > 0.05
shapiro.test(log(EsperanzaDeVida))$p.value > 0.05
shapiro.test(log(Desempleo))$p.value > 0.05
shapiro.test(log(PNBperCapita))$p.value > 0.05
shapiro.test(log(DeflactorPIB))$p.value > 0.05

# 6. 
# 7.

# Ahora se aplica el analisis de correlación de Pearson

# CONSULTAR

logIDH <- log(IDH)
logEV <- log(EsperanzaDeVida)
logIDH[is.na(logIDH)] <- 0
logEV[is.na(logEV)] <- 0

correlation <- cor(logIDH, logEV, method = 'pearson')




