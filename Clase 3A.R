# Librer?as
library(GPArotation)
library(readxl)#Leer excel
library(psych) #Datos descriptivos y m?s
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
library(corrr)
library(foreign)
library(haven)
library(MVN)
library(lavaanPlot)
library(kableExtra)
library(gptstudio)
library(shiny)
install.packages(c("httr", "stringr"))

library(httr)
library(stringr)


##----------------------------------------------------------------------------------------------------------
#Importar base de datos en da
da=ESCALA_TMMS_24
##----------------------------------------------------------------------------------------------------------
#Colocar la ruta donde se exportar?n las hojas de c?lculo 
setwd('F:/Psicologia/Analisis en R')#/Ejemplo 

#Importar datos
da <- read_sav("D:/Bases de datos/ESCALA TMMS-24.sav")

#Ver variables
names(da)

# Creando un subconjunto de índices de ajuste que examinaremos en todos los modelos
fit.subset<-c("chisq.scaled", "pvalue.scaled",
              "df.scaled","cfi.scaled", 
              "tli.scaled", "rmsea.scaled",
              "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
              "srmr","wrmr")

##----------------------------------------------------------------------------------------------------------
#Análisis factorial confirmatorio
# Probaremos modelos
#1) Modelo unidimensional congenerico
UNI.CFA.CONG<-'G=~E1+E2+E3+E4+E5+E6+E7+E8+E9+
E10+E11+E12+E13+E14+E15+E16+E17+E18+E19+E20
+E21+E22+E23+E24
'

#Estimación
fit.UNI.CFA.CONG<-cfa(model = UNI.CFA.CONG, data =da,estimator="WLSMV", 
                      mimic="Mplus", ordered = T) #std.lv=TRUE Colocar si es necesario igualar a 1 la regresi?n

# Evaluación del modelo
fitmeasures(fit.UNI.CFA.CONG, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "UNIFAC", model = fit.UNI.CFA.CONG,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.UNI.CFA.CONG, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.5, label.prop=2)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.UNI.CFA.CONG, standardized = T)[1:24,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.UNI.CFA.CONG, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificaciÓn
modindices(fit.UNI.CFA.CONG,sort=TRUE, maximum.number = 15)

#Confiabilidad
Real1<-reliability(fit.UNI.CFA.CONG,return.total = TRUE)
Real1<-round(Real1,3)
Real1

#2) Modelo unidimensional tau equivalente (PUEDEN SALTERSE)
UNI.CFA.TAU<-'G=~1*E1+1*E2+1*E3+1*E4+1*E5+1*E6+1*E7+1*E8+
1*E9+1*E10+1*E11+1*E12+1*E13+1*E14+1*E15+1*E16+
1*E17+1*E18+1*E19+1*E20+1*E21+1*E22+1*E23+1*E24
'

#Estimación
fit.UNI.CFA.TAU<-cfa(model = UNI.CFA.TAU, data =da,estimator="WLSMV", 
                      mimic="Mplus",ordered=names(da))

# Evaluación del modelo
fitmeasures(fit.UNI.CFA.TAU, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "UNIFACTAU", model = fit.UNI.CFA.TAU,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.UNI.CFA.TAU, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 2,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.75, label.prop=2)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.UNI.CFA.TAU, standardized = T)[1:24,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.UNI.CFA.TAU, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificaciÓn
modindices(fit.UNI.CFA.TAU, sort=TRUE, maximum.number = 15)

#Confiabilidad
Real2<-reliability(fit.UNI.CFA.TAU, what = c("alpha", "alpha.ord"))
Real2<-round(Real2,3)
Real2

#3) Modelo multidimensional de 3 factores correlacionados
FC3.CFA<-'F1=~E1+E2+E3+E4+E5+E6+E7+E8
F2=~E9+E10+E11+E12+E13+E14+E15+E16
F3=~E17+E18+E19+E20+E21+E22+E23+E24'

#Estimación
fit.FC3.CFA<-cfa(model = FC3.CFA, data =da, estimator="WLSMV", 
                      mimic="Mplus",ordered=names(da)) #std.lv=TRUE Colocar si es necesario igualar a 1 la regresi?n

# ML = N M = Prueba Mardi, Linea 
#MLM = 5 a más opciones de respuesta 
#MLR = 5 a más opciones de respuesta
#MLMV = 5 a más opciones de respuesta

#ULS =  5 a menos  
#ULSMV =  5 a menos Modelo miento

#WLSMV =  5 a menos 

# Evaluación del modelo
fitmeasures(fit.FC3.CFA, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "UNIFAC", model = fit.FC3.CFA,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.FC3.CFA, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 1, label.prop=2)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.FC3.CFA, standardized = T)[1:24,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.FC3.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificaciÓn
modindices(fit.FC3.CFA, sort=TRUE, maximum.number = 15)

#Confiabilidad
Real3<-reliability(fit.FC3.CFA, what = c("omega3", "ave"))
Real3<-round(Real3,3)
Real3

#4) Modelo multidimensional de 3 factores incorrelacionados
FI3.CFA<-'F1=~E1+E2+E3+E4+E5+E6+E7+E8
F2=~E9+E10+E11+E12+E13+E14+E15+E16
F3=~E17+E18+E19+E20+E21+E22+E23+E24
'
#Estimación
fit.FI3.CFA<-cfa(model = FI3.CFA, data =da,estimator="WLSMV", 
                 mimic="Mplus",ordered=names(da), orthogonal=TRUE) #std.lv=TRUE Colocar si es necesario igualar a 1 la regresi?n

# Evaluación del modelo
fitmeasures(fit.FI3.CFA, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "FACINC", model = fit.FI3.CFA,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.FI3.CFA, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.7, label.prop=2)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.FI3.CFA, standardized = T)[1:24,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.FI3.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificaciÓn
modindices(fit.FC3.CFA, sort=TRUE, maximum.number = 15)

#Confiabilidad
Real4<-reliability(fit.FI3.CFA, what = c("omega3", "ave"))
Real4<-round(Real4,3)
Real4

#5) Modelo Multidimensional de segundo orden
L2.CFA<-'F1=~E1+E2+E3+E4+E5+E6+E7+E8
F2=~E9+E10+E11+E12+E13+E14+E15+E16
F3=~E17+E18+E19+E20+E21+E22+E23+E24
G =~ 1*F1 + 1*F2 + 1*F3 
G ~~ G'


#Estimación
fit.L2.CFA<-cfa(model = L2.CFA, data =da,estimator="WLSMV", 
                 mimic="Mplus",ordered=names(da)) #std.lv=TRUE Colocar si es necesario igualar a 1 la regresi?n

# Evaluación del modelo
fitmeasures(fit.L2.CFA, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "L2", model = fit.L2.CFA,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.L2.CFA, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.7, label.prop=2)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.L2.CFA, standardized = T)[1:24,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.L2.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificaciÓn
modindices(fit.L2.CFA, sort=TRUE, maximum.number = 15)
#Se sugiere usarlo en escalas con tres o más factores

###### Transformación Shidmt-Leiman
#Para modelos de segundo orden
#Transformaci?n de Shmit-
SL_lav <- SL(fit.L2.CFA, g_name = "G")
SL_lav$sl

#Confiabilidad
OMEGA(fit.L2.CFA, g_name = "G")

#6) Modelo Multidimensional Bifactor
BIFAC.CFA<-'F1=~E1+E2+E3+E4+E5+E6+E7+E8
F2=~E9+E10+E11+E12+E13+E14+E15+E16
F3=~E17+E18+E19+E20+E21+E22+E23+E24
G=~E1+E2+E3+E4+E5+E6+E7+E8+E9+E10+E11+E12+E13+
E14+E15+E16+E17+E18+E19+E20+E21+E22+E23+E24'

#Estimación
fit.BIFAC.CFA<-cfa(model = BIFAC.CFA, data = da, orthogonal= TRUE, 
         estimator="WLSMV", mimic="Mplus", std.lv=TRUE, ordered=names(da))

# Evaluación del modelo
fitmeasures(fit.BIFAC.CFA, fit.subset)

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "BIFAC", model = fit.BIFAC.CFA,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "black"), coefs = TRUE)

#Path analisis
semPaths(fit.BIFAC.CFA, whatLabels= "std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", edge.width = 1.5,nDigits = 2, 
         edge.label.cex = 0.6,label.prop= 2, bifactor=TRUE)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.BIFAC.CFA, standardized = T)[1:48,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

#Resumen
summary(fit.BIFAC.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Indice de modificación
modindices(fit.BIFAC.CFA, sort=TRUE, maximum.number = 15)

#Confiabilidad
OMEGA(fit.BIFAC.CFA, g_name = "G")

#---------------------------------------------------------------------------------------------------------------------
#Evaluación de los modeloa
fit.summary <- round(rbind(fitmeasures(fit.UNI.CFA.CONG, fit.subset),
                           fitmeasures(fit.UNI.CFA.TAU, fit.subset),
                           fitmeasures(fit.FC3.CFA, fit.subset),
                           fitmeasures(fit.FI3.CFA, fit.subset),
                           fitmeasures(fit.L2.CFA, fit.subset),
                           fitmeasures(fit.BIFAC.CFA, fit.subset)),3)

rownames(fit.summary) <- c("Congenerico", "Tau equivalente", "Factores relacionados", "Factores no relacionados", "2do orden","Bifactor")
colnames(fit.summary) <- c("χ²","p","gl","CFI","TLI","RMSEA","Lower","Upper","SRMR","WRMR")
fit.summary

#Guardar
fit.summary <- as.data.frame(fit.summary)
fit.summary
write.xlsx(fit.summary,"Modelos2.xlsx",colNames=TRUE, rowNames=TRUE)



