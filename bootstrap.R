library("TOSTER")
library("dplyr")

set.seed(7)
###############################################Africa
data <- df_AfricaMaqueta

IDH_Africa <- df_AfricaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Africa <- df_AfricaMaqueta$`Esperanza de vida al nacer`
Desempleo_Africa <- df_AfricaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Africa <- df_AfricaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Africa <- df_AfricaMaqueta$`Deflactor de PIB`


#EVN
IDH_EVNP <-boot_cor_test(
  IDH_Africa,
  EsperanzaDeVida_Africa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_EVNS<-boot_cor_test(
  IDH_Africa,
  EsperanzaDeVida_Africa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_EVNW<-boot_cor_test(
  IDH_Africa,
  EsperanzaDeVida_Africa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_EVN <- data.frame(c(IDH_EVNP,IDH_EVNS,IDH_EVNW))
colnames(temp_EVN)[c(1, 2,3)] <- c("IDH_EVNP" ,
                                   "IDH_ENVS",
                                   "IDH_ENVW")


#Desempleo
IDH_DesP <-boot_cor_test(
  IDH_Africa,
  Desempleo_Africa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_DesS<-boot_cor_test(
  IDH_Africa,
  Desempleo_Africa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_DesW<-boot_cor_test(
  IDH_Africa,
  Desempleo_Africa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Des <- data.frame(c(IDH_DesP,IDH_DesS,IDH_DesW))
colnames(temp_Des)[c(1, 2,3)] <- c("IDH_DesP" ,
                                   "IDH_DesS",
                                   "IDH_DesW")

# PNB
IDH_PNBP <-boot_cor_test(
  IDH_Africa,
  PNBperCapita_Africa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_PNBS<-boot_cor_test(
  IDH_Africa,
  PNBperCapita_Africa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_PNBW<-boot_cor_test(
  IDH_Africa,
  PNBperCapita_Africa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_PNB <- data.frame(c(IDH_PNBP,IDH_PNBS,IDH_PNBW))
colnames(temp_PNB)[c(1, 2,3)] <- c("IDH_PNBP" ,
                                   "IDH_PNBS",
                                   "IDH_PNBW")

# Deflactor
IDH_InfP <-boot_cor_test(
  IDH_Africa[-c(38)],
  DeflactorPIB_Africa[-c(38)],
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_InfS<-boot_cor_test(
  IDH_Africa,
  DeflactorPIB_Africa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_InfW<-boot_cor_test(
  IDH_Africa,
  DeflactorPIB_Africa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Inf <- data.frame(c(IDH_InfP,IDH_InfS,IDH_InfW))
colnames(temp_Inf)[c(1, 2,3)] <- c("IDH_InfP" ,
                                   "IDH_InfS",
                                   "IDH_InfW")

Boot_Africa<-cbind(temp_EVN, temp_Des, temp_PNB,temp_Inf)
rownames(Boot_Africa) <- c("Lower_Africa","Upper_Africa") 


###################################################Asia
df_AsiaMaqueta <- na.omit(df_Asialn)
row.names(df_AsiaMaqueta) <- NULL

IDH_Asia <- df_AsiaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Asia <- df_AsiaMaqueta$`Esperanza de vida al nacer`
Desempleo_Asia <- df_AsiaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Asia <- df_AsiaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Asia <- df_AsiaMaqueta$`Deflactor de PIB`

#EVN
IDH_EVNP <-boot_cor_test(
  IDH_Asia,
  EsperanzaDeVida_Asia,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_EVNS<-boot_cor_test(
  IDH_Asia,
  EsperanzaDeVida_Asia,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_EVNW<-boot_cor_test(
  IDH_Asia,
  EsperanzaDeVida_Asia,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_EVN <- data.frame(c(IDH_EVNP,IDH_EVNS,IDH_EVNW))
colnames(temp_EVN)[c(1, 2,3)] <- c("IDH_EVNP" ,
                                   "IDH_ENVS",
                                   "IDH_ENVW")

#Desempleo
IDH_DesP <-boot_cor_test(
  IDH_Asia,
  Desempleo_Asia,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_DesS<-boot_cor_test(
  IDH_Asia,
  Desempleo_Asia,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_DesW<-boot_cor_test(
  IDH_Asia,
  Desempleo_Asia,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Des <- data.frame(c(IDH_DesP,IDH_DesS,IDH_DesW))
colnames(temp_Des)[c(1, 2,3)] <- c("IDH_DesP" ,
                                   "IDH_DesS",
                                   "IDH_DesW")

# PNB
IDH_PNBP <-boot_cor_test(
  IDH_Asia,
  PNBperCapita_Asia,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_PNBS<-boot_cor_test(
  IDH_Asia,
  PNBperCapita_Asia,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_PNBW<-boot_cor_test(
  IDH_Asia,
  PNBperCapita_Asia,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_PNB <- data.frame(c(IDH_PNBP,IDH_PNBS,IDH_PNBW))
colnames(temp_PNB)[c(1, 2,3)] <- c("IDH_PNBP" ,
                                   "IDH_PNBS",
                                   "IDH_PNBW")

# Deflactor
IDH_InfP <-boot_cor_test(
  IDH_Asia,
  DeflactorPIB_Asia,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_InfS<-boot_cor_test(
  IDH_Asia,
  DeflactorPIB_Asia,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_InfW<-boot_cor_test(
  IDH_Asia,
  DeflactorPIB_Asia,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Inf <- data.frame(c(IDH_InfP,IDH_InfS,IDH_InfW))
colnames(temp_Inf)[c(1, 2,3)] <- c("IDH_InfP" ,
                                   "IDH_InfS",
                                   "IDH_InfW")
Boot_Asia<-cbind(temp_EVN, temp_Des, temp_PNB,temp_Inf)
rownames(Boot_Asia) <- c("Lower_Asia","Upper_Asia") 

#####################################################America
df_AmericaMaqueta <- na.omit(df_Americaln)
row.names(df_AmericaMaqueta) <- NULL

IDH_America <- df_AmericaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_America <- df_AmericaMaqueta$`Esperanza de vida al nacer`
Desempleo_America <- df_AmericaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_America <- df_AmericaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_America <- df_AmericaMaqueta$`Deflactor de PIB`

#EVN
IDH_EVNP <-boot_cor_test(
  IDH_America[c(-16)], 
  EsperanzaDeVida_America[c(-16)],
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_EVNS<-boot_cor_test(
  IDH_America,
  EsperanzaDeVida_America,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_EVNW<-boot_cor_test(
  IDH_America,
  EsperanzaDeVida_America,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_EVN <- data.frame(c(IDH_EVNP,IDH_EVNS,IDH_EVNW))
colnames(temp_EVN)[c(1, 2,3)] <- c("IDH_EVNP" ,
                                   "IDH_ENVS",
                                   "IDH_ENVW")

#Desempleo
IDH_DesP <-boot_cor_test(
  IDH_America[c(-16)], 
  Desempleo_America[c(-16)],
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_DesS<-boot_cor_test(
  IDH_America,
  Desempleo_America,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_DesW<-boot_cor_test(
  IDH_America,
  Desempleo_America,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Des <- data.frame(c(IDH_DesP,IDH_DesS,IDH_DesW))
colnames(temp_Des)[c(1, 2,3)] <- c("IDH_DesP" ,
                                   "IDH_DesS",
                                   "IDH_DesW")
# PNB
IDH_PNBP <-boot_cor_test(
  IDH_America[c(-16)], 
  PNBperCapita_America[c(-16)],
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_PNBS<-boot_cor_test(
  IDH_America,
  PNBperCapita_America,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_PNBW<-boot_cor_test(
  IDH_America,
  PNBperCapita_America,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_PNB <- data.frame(c(IDH_PNBP,IDH_PNBS,IDH_PNBW))
colnames(temp_PNB)[c(1, 2,3)] <- c("IDH_PNBP" ,
                                   "IDH_PNBS",
                                   "IDH_PNBW")
# Deflactor
IDH_InfP <-boot_cor_test(
  IDH_America[c(-1,-16)], 
  DeflactorPIB_America[c(-1,-16)],
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_InfS<-boot_cor_test(
  IDH_America, 
  DeflactorPIB_America,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_InfW<-boot_cor_test(
  IDH_America, 
  DeflactorPIB_America,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Inf <- data.frame(c(IDH_InfP,IDH_InfS,IDH_InfW))
colnames(temp_Inf)[c(1, 2,3)] <- c("IDH_InfP" ,
                                   "IDH_InfS",
                                   "IDH_InfW")


Boot_America<-cbind(temp_EVN, temp_Des, temp_PNB,temp_Inf)
rownames(Boot_America) <- c("Lower_America", "Upper_America") 

################################################Europa
df_EuropaMaqueta <- na.omit(df_Europaln)
row.names(df_EuropaMaqueta) <- NULL

IDH_Europa <- df_EuropaMaqueta$`Indice de desarrollo humano`
EsperanzaDeVida_Europa <- df_EuropaMaqueta$`Esperanza de vida al nacer`
Desempleo_Europa <- df_EuropaMaqueta$`Desempleo total (% del total de la fuerza laboral)`
PNBperCapita_Europa <- df_EuropaMaqueta$`Producto Nacional Bruto per capita`
DeflactorPIB_Europa <- df_EuropaMaqueta$`Deflactor de PIB`

#EVN
IDH_EVNP <-boot_cor_test(
  IDH_Europa,
  EsperanzaDeVida_Europa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_EVNS<-boot_cor_test(
  IDH_Europa,
  EsperanzaDeVida_Europa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_EVNW<-boot_cor_test(
  IDH_Europa,
  EsperanzaDeVida_Europa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_EVN <- data.frame(c(IDH_EVNP,IDH_EVNS,IDH_EVNW))
colnames(temp_EVN)[c(1, 2,3)] <- c("IDH_EVNP" ,
                                   "IDH_ENVS",
                                   "IDH_ENVW")
#Desempleo
IDH_DesP <-boot_cor_test(
  IDH_Europa,
  Desempleo_Europa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_DesS<-boot_cor_test(
  IDH_Europa,
  Desempleo_Europa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_DesW<-boot_cor_test(
  IDH_Europa,
  Desempleo_Europa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Des <- data.frame(c(IDH_DesP,IDH_DesS,IDH_DesW))
colnames(temp_Des)[c(1, 2,3)] <- c("IDH_DesP" ,
                                   "IDH_DesS",
                                   "IDH_DesW")

# PNB
IDH_PNBP <-boot_cor_test(
  IDH_Europa,
  PNBperCapita_Europa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_PNBS<-boot_cor_test(
  IDH_Europa,
  PNBperCapita_Europa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_PNBW<-boot_cor_test(
  IDH_Europa,
  PNBperCapita_Europa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_PNB <- data.frame(c(IDH_PNBP,IDH_PNBS,IDH_PNBW))
colnames(temp_PNB)[c(1, 2,3)] <- c("IDH_PNBP" ,
                                   "IDH_PNBS",
                                   "IDH_PNBW")

# Deflactor
IDH_InfP <-boot_cor_test(
  IDH_Europa,
  DeflactorPIB_Europa,
  alternative = c("two.sided"),
  method = c("pearson"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
IDH_InfS<-boot_cor_test(
  IDH_Europa,
  DeflactorPIB_Europa,
  alternative = c("two.sided"),
  method = c("spearman"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]

IDH_InfW<-boot_cor_test(
  IDH_Europa,
  DeflactorPIB_Europa,
  alternative = c("two.sided"),
  method = c( "winsorized"),
  alpha = 0.05,
  null = 0,
  R = 1000
)[3]
temp_Inf <- data.frame(c(IDH_InfP,IDH_InfS,IDH_InfW))
colnames(temp_Inf)[c(1, 2,3)] <- c("IDH_InfP" ,
                                   "IDH_InfS",
                                   "IDH_InfW")
Boot_Europa<-cbind(temp_EVN, temp_Des, temp_PNB,temp_Inf)
rownames(Boot_Europa) <- c("Lower_Europa", "Upper_Europa") 

bootstrap<- rbind(Boot_Africa, Boot_America, Boot_Asia, Boot_Europa)
data<-as.data.frame(bootstrap)
data<-tibble::rownames_to_column(data, "rn")

p <-t(bootstrap)
data<-as.data.frame(p)
data<-tibble::rownames_to_column(data, "rn")


write_xlsx(data, "C:/Users/Ana/Desktop/II-2023/Estadistica I, CA0303/Proyecto/Bases de datos/bootseed.xlsx")
