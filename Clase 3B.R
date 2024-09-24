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
library(nFactors)
library(see)
library(haven)
install.packages("see")
citation("stats")
##----------------------------------------------------------------------------------------------------------
#Importar base de datos en da
da=ESCALA_TMMS_24

#Colocar la ruta donde se exportar?n las hojas de cálculo 
setwd('F:/Psicologia/Analisis en R')#/Ejemplo 

#Importar datos
da <- read_sav("F:/Psicologia/Analisis en R/ESCALA TMMS-24.sav")

#Ver variables
names(da)
da=ESCALA_TMMS_24
##----------------------------------------------------------------------------------------------------------
#Aislar la escala principal
de<-data.frame(cbind(da$E1,da$E2,da$E3,da$E4,da$E5,da$E6,da$E7,da$E8
                    ,da$E9,da$E10,da$E11,da$E12,da$E13,da$E14,da$E15,da$E16
                    ,da$E17,da$E18,da$E19,da$E20,da$E21,da$E22,da$E23,da$E24))

##----------------------------------------------------------------------------------------------------------
#Análisis Factorial Exploratorio
#Probar el supuesto previo al AFE
#Supuestos del AFE
Matriz_G<-polychoric(de)

#Prueba de adecuación muestra Kaiser-Meyer-Olkin
KMO<- KMO(de)
KMO<- KMO$KMO
KMO

#Prueba de esfericidad de Bartlett
Bartlett<- bartlett.test(de)
Bartlett

#Determinante de la matriz
Determinante <- det(Matriz_G$rho)
Determinante


#Retención de factores
results_nfactor<-n_factors(de, rotation = "oblimin", algorithm = "minres", n = NULL)
plot(results_nfactor)
results_nfactor
as.data.frame(results_nfactor)
summary(results_nfactor)

#Exploratory Factorial ANalysis
Scalefactor<-fa(de,nfactors = 3,fm = "minres",rotate ="oblimin",cor = "poly")#
print(Scalefactor,digits = 2,sort=TRUE, cut=.30)

#Rotaciones
#oblimin=Factores correlacionados
#varimax=Factores incorrelacionados
#promin=Combinacion de los anteriores metodos de rotaci?n
#---------------------------------------------------------------------------------------------------
