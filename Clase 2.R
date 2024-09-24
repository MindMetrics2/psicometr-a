# Librer?as
library(GPArotation)
library(readxl)#Leer excel
library(psych) #Datos descriptivos y m?s
library(dplyr) #Funci?n %>%
library(lavaan)#AFC y SEM
library(semTools)#Invarianza
library(parameters)#n_factors
library(semPlot)#graficos de aFC
library(EFAtools)#omega Y AFE
library(readxl)#leer
library(MBESS)#intervalos de confianza del omega
library(openxlsx)#Guardar
library(MVN)#normalidad
library(PerformanceAnalytics)#Grafico de las correlaciones
library(corrr)
library(foreign)
library(haven)
library(MVN)
library(gptstudio)

#Referencias
citation("semTools")
##----------------------------------------------------------------------------------------------------------
#Colocar la ruta donde se exportar?n las hojas de c?lculo 
setwd('D:/Taller')# F:/Psicologia/Analisis en R

#Importar datos
da <- read_sav("D:/Taller/ESCALA TMMS-24.sav")

#Ver variables
names(da)

##----------------------------------------------------------------------------------------------------------
#Análisis de ítems
#Extraer parte de la base y crear un objeto
F1 <-data.frame(cbind(da$E1, da$E2, da$E3, da$E4, da$E5, da$E6, da$E7, da$E8))#A?adir los ítems del primer factor
F2 <-data.frame(cbind(da$E9, da$E10, da$E11, da$E12, da$E13, da$E14, da$E15, da$E16))
F3 <-data.frame(cbind(da$E17, da$E18, da$E19, da$E20, da$E21, da$E22, da$E23, da$E24))


#Modifica
#Aislar la escala principal
de <-data.frame(cbind(da$E1, da$E2, da$E3, da$E4, da$E5, da$E6, da$E7, da$E8,
                      da$E9, da$E10, da$E11, da$E12, da$E13, da$E14, da$E15, da$E16,
                      da$E17, da$E18, da$E19, da$E20, da$E21, da$E22, da$E23, da$E24))


##---------------------------------------------------------------------------------------------------------
#Porcentaje de respuesta por ítem (F, %, M,DE, g1, g2, ihc, h2, id)
Tabla1<- rbind(table(F1$X1),table(F1$X2),table(F1$X3),table(F1$X4),table(F1$X5)
               ,table(F1$X6),table(F1$X7), table(F1$X8))#,table(F1$X9)

#Modificar               
Tabla2<-prop.table(Tabla1, margin = 1)
TablaFrecuencia = Tabla2*100
TablaFrecuencia 
#Sobrescribir y crear un objeto
TablaFrecuencia <-as.data.frame(TablaFrecuencia)
TablaFrecuencia <- round(TablaFrecuencia,2) 
TablaFrecuencia
#Reemplazar por el factor que se desea evaluar

##----------------------------------------------------------------------------------------------------------
#Creación de objetos para el análisis de ítem
Descriptivos<-describe(F1)  #Para M, DE, G1, G2


#Coeficiente de variación
CV <- (Descriptivos$sd /  Descriptivos$mean *100)
CV

#Para el IHC
Matriz_G<-polychoric(F1)#corr(F1)= Matriz Pearson, tetrachoric(F1)
Matriz_G
AlfaGeneral<-psych::alpha(Matriz_G$rho) #Para el IHC
AlfaGeneral

#AFE para comunalidad
AFEfactor<-fa(F1,nfactors = 1,fm = "minres",rotate ="oblimin",cor = "poly")# de querer usar una matriz tetracorica "tet"
AFEfactor#Crear tabla con los datos que se necesitar?n en el an?lisis de ?tems

# Crear tabla (Modificar segun cantidad de ítems en la dimensión list(c(1:?)
TablaAnalisis <- list(c(1:8),Descriptivos$mean,Descriptivos$sd, Descriptivos$skew,Descriptivos$kurtosis,
                      AlfaGeneral$item.stats$r.drop, AlfaGeneral$alpha.drop$std.alpha,AFEfactor$communality, CV)

TablaAnalisis <-as.data.frame(TablaAnalisis)
TablaAnalisis <- TablaAnalisis[,-1]
#Dar formato a los resultados
TablaAnalisis <- TablaAnalisis %>% 
  mutate_if(is.numeric, round, digits = 2)
#Nombrar y exportar en Excel
names(TablaAnalisis)<- c("M","DE","g1","g2","IHC","Alfa.drop","h2","CV")
TablaAnalisis

#AlfaGeneral$item.stats$r.cor#para alf si se elimina de la escalo o factor
##----------------------------------------------------------------------------------------------------------
#Concatenar ambos resultados para la tabla final de an?lisis de ?tems
TablaFinal <- list(cbind(TablaFrecuencia,TablaAnalisis))
TablaFinal
write.xlsx(TablaFinal, "F1.xlsx", colNames=TRUE, rowNames=TRUE)
##----------------------------------------------------------------------------------------------------------
#EMPEZAR LUEGO DE HABER EJECUTADO TODAS LAS DIMENSIONES
#Matriz de correlación policóricas
Matriz_G<-polychoric(de)
ImprimirMatriz <- as.data.frame(Matriz_G$rho)
ImprimirMatriz <- ImprimirMatriz %>% 
  mutate_if(is.numeric, round, digits = 2)
write.xlsx(ImprimirMatriz, "matriz.xlsx")


#Gráfico
#Renombrar
names(de)<- c("ÍTEM1","ÍTEM2","ÍTEM3","ÍTEM4","ÍTEM5",
              "ÍTEM6","ÍTEM7","ÍTEM8","ÍTEM9","ÍTEM10",
              "ÍTEM11","ÍTEM12","ÍTEM13","ÍTEM14","ÍTEM15",
              "ÍTEM16","ÍTEM17","ÍTEM18","ÍTEM19","ÍTEM20",
              "ÍTEM21","ÍTEM22","ÍTEM23","ÍTEM24")

Matriz_G<-polychoric(de)
Matriz_G$rho

#red de correlaciones
Graf<- Matriz_G$rho%>% 
  network_plot()
Graf

#Matriz de correlaci?n pearson
Matriz_G<-cor(de)
ImprimirMatriz <- as.data.frame(Matriz_G)
ImprimirMatriz <- ImprimirMatriz %>% 
  mutate_if(is.numeric, round, digits = 2)
write.xlsx(ImprimirMatriz, "Matrizpearso.xlsx")

#red de correlaciones
Matriz_Go%>% 
  network_plot()

#Matriz de correlaci?n matrices tetratoc?ricas
Matriz_G<-tetrachoric(di)
ImprimirMatriz <- as.data.frame(Matriz_G$rho)
ImprimirMatriz <- ImprimirMatriz %>% 
  mutate_if(is.numeric, round, digits = 2)
write.xlsx(ImprimirMatriz, "Matriztetratorica.xlsx")

#red de correlaciones
Matriz_Go%>% 
  network_plot()

#Matrices de correlaci?n
#cor = "poly" #Policorica
#cor = "tetrachoric" #Tetracorica
#Se asume matriz Pearson cuando no se especifica (borrar cor = " ")

library(corrplot)
corrplot(Matriz_G$rho, type="upper", tl.col="black", tl.srt=90, cex.var=0.4)
# Poly y tetra: Matriz_G$rho  o Pearson: Matriz_G

##----------------------------------------------------------------------------------------------------------
#Normalidad multivariada
mvn(de, mvnTest = "mardia")

#Normalidad univariada
uni <- mvn(de, univariateTest = "SW")
uni
