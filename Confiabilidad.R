# Librer?as
library(GPArotation)
library(readxl)#Leer excel
library(psych) #Datos descriptivos y m?s
library(xlsx)  #Exportar a Excel
library(dplyr) #Funci?n %>%
library(lavaan)#AFC
library(semTools)#Invarianza
library(parameters)#n_factors
library(semPlot)#graficos de aFC
library(EFAtools)#omega Y AFE
library(readxl)#leer
library(MBESS)#intervalos de confianza del omega
library(openxlsx)#Guardar
library(MVN)#normalidad
library(PerformanceAnalytics)#Grafico de las correlaciones
##----------------------------------------------------------------------------------------------------------
#Importar base de datos en da
da=ESCALA_TMMS_24
names(da)
##----------------------------------------------------------------------------------------------------------
#Aislar la escala principal
General<-data.frame(cbind(da$E1,da$E2,da$E3,da$E4,da$E5,da$E6,da$E7,da$E8
                          ,da$E9,da$E10,da$E11,da$E12,da$E13,da$E14,da$E15,da$E16
                          ,da$E17,da$E18,da$E19,da$E20,da$E21,da$E22,da$E23,da$E24))
de=General

#A?adir tantos factores como sea necesario

#Colocar la ruta donde se exportar?n las hojas de c?lculo 
setwd('F:/Psicologia/Analisis en R/Ejemplo')

##----------------------------------------------------------------------------------------------------------
#Cargas factoriales estandarizadas
Cargas <- standardizedSolution(fit.BIFAC.CFA)
Cargas

#Guardar cargas factoriales estandarizadas en excel
Cargas <- as.data.frame(Cargas)
Cargas
write.xlsx(Cargas, "bifac.xlsx")


#---------------------------------------------------------------------------------------------------
#Confiabilidad para escalas con estructuras unifactoriales, multidimensionales de factores correlacionados, incorrelacionados
fiabilidad<-reliability(fit.L2.CFA,return.total = TRUE)
fiabilidad

#Confiabilidad para escalas con estructuras unifactoriales con intervalos de confianza
ci.reliability(data=de, type = "alpha")


#Modelos jerárquicos
#Confiabilidad para escalas con estructuras multidimensionales con un factor de segundo orden
reliabilityL2(fit, 'G')


#Para modelos de segundo orden
#Transformación de Shmit- lEIA
SL_lav <- SL(fit, g_name = "G")
SL_lav 
#Indicar el nombre del factor general para obtener las cargas factoriales mediante la transformaci?n Schmit-Leiman

# Compute omega for second-order solution or bifactor
OMEGA(fit.Modelo6, g_name = "FG")
#Para calcular el omega apartir de las cargas factoriales de la transformaci?n Schmit-Leiman
#---------------------------------------------------------------------------------------------------


