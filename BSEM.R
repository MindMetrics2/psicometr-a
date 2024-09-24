library(usethis)    # extra
library(devtools)    # For version control
library(dplyr)       # For data management 
library(GPArotation) # For rotation used in factor analysis
library(lavaan)      # For SEM
library(semPlot)
library(lavaanPlot)  # For plotting SEM
library(psych)       # For data descriptives and reliability statistics
library(reshape2)    # For data manipulation
library(rlang)       # For string manipulation
library(semTools)    # For factor analysis
library(haven)

#Asociar con una carpeta donde exporte los resultados
setwd('F:/Psicologia/Analisis en R')

#Importar base de datos
da <- read_sav("D:/Bases de datos/ESCALA TMMS-24-1.sav")

#En caso de importar mediante Import dataset se debe simplificar el nombre del dataframe
da=ESCALA_TMMS_24_1

#Ver variables
names(da)

# Ejecutar descriptivos
describe(da)

# Correlación entre ítems matriz policórica
round(polychoric(da)$rho,2)

poly <- polychoric(da)$rho 

#Matriz Pearson
round(cor(da, use = "pairwise.complete.obs"),2)

#Coeficiente omega del primer factor
omega(da[1:8], nfactors = 1)$omega.tot

#Coeficiente omega del segundo factor
omega(da[9:16], nfactors = 1)$omega.tot

#Coeficiente omega del tercer factor
omega(da[17:24], nfactors = 1)$omega.tot


#Especificación de modelos teóricamente plausible
#UNIDIMENSIONAL
UNI.CFA <- 'G=~ A1+A2+A3+A4+A5+A6+A7+A8+
B1+B2+B3+B4+B5+B6+B7+B8+
C1+C2+C3+C4+C5+C6+C7+C8'

#Estimación del modelo mediante AFC
fit.UNI.CFA <- cfa(model = UNI.CFA,
                   data = da, estimator = "WLSMV", ordered = TRUE) 


lavaanPlot(name = "UnidimensionalCFA",
           model = fit.UNI.CFA)

# Creando un subconjunto de índices de ajuste que examinaremos en todos los modelos
fit.subset<-c("chisq.scaled", "pvalue.scaled",
              "df.scaled","cfi.scaled", 
              "tli.scaled", "rmsea.scaled",
              "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","wrmr")

# Extrayendo índices de ajuste del primer modelo
fitmeasures(fit.UNI.CFA, fit.subset)

#Path analisis
semPaths(fit.UNI.CFA, whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.7, label.prop=2)

#Resumen
summary(fit.UNI.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Modelo de factores correlacionados
# Construimos el modelo
FA3.CFA<-'F1=~A1+A2+A3+A4+A5+A6+A7+A8
F2=~B1+B2+B3+B4+B5+B6+B7+B8
F3=~C1+C2+C3+C4+C5+C6+C7+C8'

#Estimación del modelo mediante AFC
fit.FA3.CFA <- cfa(model = FA3.CFA,
                   data = da, estimator = "WLSMV", ordered = TRUE) 

#Gráficos
lavaanPlot(name = "CFA",
           model = fit.FA3.CFA)

#Path analisis
semPaths(fit.FA3.CFA, what = "std", whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.7,label.prop=2)

# Extrayendo índices de ajuste del primer modelo
fitmeasures(fit.FA3.CFA, fit.subset)

#Resumen
summary(fit.FA3.CFA, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#BIFACTOR
# Construimos el modelo
BIFA.CFA <- 'G =~ NA*A1 + A2 + A3 + A4 + A5 + A6 +A7+A8
                      + B1 + B2 + B3 + B4 +B5 + B6 +B7+B8
                      + C1 + C2 + C3 + C4 + C5+C6+C7+C8
                F1 =~ NA*A1 + A2 + A3 + A4 + A5 + A6+A7+A8
                F2 =~ NA*B1 + B2 + B3 + B4 +B5 + B6 +B7+B8
                F3  =~ NA*C1 + C2 + C3 + C4 + C5+C6+C7+C8
                # Factors are orthogonal
                G ~~ 0*F1 + 0*F2 + 0*F3
                F1 ~~ 0*F2 + 0*F3
                F2 ~~ 0*F3
                # Setting latent variable variance at 1
                G ~~ 1*G
                F1 ~~ 1*F1
                F2 ~~ 1*F2
                F3 ~~ 1*F3'

#Estimación del modelo mediante AFC
fit.BIFA.CFA<- cfa(model = BIFA.CFA,
                   data = da,
                   # We use the maximum likelihood with robust SE estimator
                   estimator = "WLSMV",ordered = TRUE) 

# También podemos mirar la forma gráfica del modelo
lavaanPlot(name = "BifactorCFA",model = fit.BIFA.CFA,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE)

#Path analisis
semPaths(fit.BIFA.CFA, intercepts = FALSE,residuals=FALSE,edge.label.cex=1,
         sizeInt=5,edge.color ="black",esize = 1.5, label.prop=1,
         rotation = 1, sizeMan = 4,sizeLat = 8,
         layout = "tree3", style = "lisrel",nCharNodes = 0,"std")

# Extracting fit indices from the model
fitmeasures(fit.BIFA.CFA, fit.subset)


# Modelamiento de ecuaciones estructurales exploratorias ------------------
# Modelo de factores correlacionados con ESEM
# Primero, llevamos a cabo un EFA sin ninguna rotación
efa <- efaUnrotate(# Especificación del conjunto de datos
  data = da,
  # 3 factors
  nf = 3)

# A continuación, construimos una matriz objetivo para la rotación
target.mat <- as.matrix(cbind(c(rep(NA,8),rep(0,16)),
                              c(rep(0,8),rep(NA,8),rep(0,8)),
                              c(rep(0,16),rep(NA,8))))

# Ver matriz
target.mat


# Rotar la matriz de cargas EFA
rotated <- funRotate(efa, 
                     # Rotación de objetivo oblicua
                     fun = "targetQ", 
                     # Especificación de la matriz objetivo
                     Target = target.mat)

# Ver las cargas estandarizadas
rotated.loadings <- rotated@loading

# Para usar cargas como valores iniciales para el ESEM, necesitamos
# invertirlos a la métrica de los elementos. lo hacemos
# multiplicando las cargas por las desviaciones estándar de los artículos
rotated.loadings.unstd <- rotated.loadings * describe(da)$sd

#Escribir una función para generar la sintaxis del modelo.
#Debido a que el modelo se vuelve realmente complejo 
#con todos los elementos que se cargan de forma cruzada en cada variable latente, 
#escribiremos unas pocas líneas para generar nuestro modelo de lavaan.

# Manipulando la matriz de cargas
loadings.esem <- melt(round(rotated.loadings.unstd,3))

# Nombrando las columnas
colnames(loadings.esem) <- c("item", "latent", "value")

# Etiquetando nuestras variables latentes
loadings.esem$latent <- c(rep("F1",24),
                          rep("F2",24),
                          rep("F3",24))

#  Generando la sintaxis del modelo
# Primero, establecemos dos anclas para cada variable latente: una es el primer elemento
# en una subescala correspondiente y otra que no está en la misma subescala
anchors <- as.data.frame(cbind(latent = c("F1","F2","F3","F1","F2","F3"),
                               item = c("A1","B1","C1","B1","C1","A1")))

# Ver las dataframe ancla
anchors

# Creé una función para generar la sintaxis del modelo "esem.lavaan.syntax"
esem.lavaan.syntax <- function (loadings.dt, anchors){  
  #Cree la variable is_anchor en el marco de datos de anclas
  anchors$is.anchor <- 1
  
# Combine el marco de datos de anclas con el marco de datos de cargas y reorganícelo
loadings.dt <- merge(anchors, loadings.dt, by = c("item","latent"), all.y = TRUE)
loadings.dt$is.anchor[is.na(loadings.dt$is.anchor)]<-0
loadings.dt <- arrange(loadings.dt, desc(is.anchor))  
loadings.dt <- arrange(loadings.dt, latent)

# Hacer columna de sintaxis por ítems; la sintaxis es diferente dependiendo de is.anchor
loadings.dt <- loadings.dt %>%
  mutate(syntax = ifelse(is.anchor == 1, 
                         paste0(value,"*", item),
                         paste0("start(",value,")*", item)))

# Combine valores iniciales/fijos y nombres de elementos para generar una línea
# para cada variable latente
loadings.combined <- loadings.dt %>%
  group_by(latent) %>%
  summarize(syntaxline = paste(syntax, collapse = "+"))
syntaxlines <- paste(loadings.combined$latent, "=~", loadings.combined$syntaxline)
         
# Agregue líneas para fijar la varianza de la variable latente en 1
c(syntaxlines,
  paste(loadings.combined$latent, "~~ 1*", loadings.combined$latent))

}

# Echemos un vistazo a la sintaxis generada
esem.lavaan.syntax(loadings.esem, anchors)

#Estimación
fit.FA3.ESEM <- cfa(model = esem.lavaan.syntax(loadings.esem, anchors),
                    data = da,
                    # Podemos usar diversos estimadores como ML, MLR, MLM, ULS, ULSMV, WLSMV
                    estimator = "WLSMV", ordered = TRUE, mimic="Mplus")

lavaanPlot(name = "ESEM",
           model = fit.FA3.ESEM)

#Path analisis
semPaths(fit.FA3.ESEM, what = "std", whatLabels="std",
         style = "lisrel", layout = "tree3", 
         intercepts = FALSE, residuals = FALSE, thresholds = FALSE,
         rotation = 1, sizeMan = 3,sizeLat = 8, shapeMan = "rectangle", shapeLat = "circle",
         edge.color ="black", nDigits = 2, edge.label.cex = 0.8,label.prop=3.2)

#Resumen
summary(fit.FA3.ESEM, fit.measures = TRUE, standardized=T, rsquare=TRUE)

# Echemos un vistazo a las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.FA3.ESEM, standardized = T)[1:72,], rhs),lhs),
       # solo ver un subconjunto de resultados de interés
       lhs:est,pvalue,std.all)

# Extrayendo índices de ajuste del modelo
fitmeasures(fit.FA3.ESEM, fit.subset)

#Resumen
summary(fit.FA3.ESEM, fit.measures = TRUE, standardized=T, rsquare=TRUE)

## Modelo bifactor con ESEM
# Primero, llevamos a cabo un EFA sin ninguna rotación.
efa.bifac <- efaUnrotate(data = da, estimator = "wlsmv", ordered=T,
                         # 4 factors
                         nf = 4)
summary(efa.bifac, std = TRUE)
inspect(efa.bifac, "std")


# Construcción de una matriz objetivo
target.mat.bifac <- as.matrix(cbind(rep(NA,24),
                                    c(rep(NA,8),rep(0,16)),
                                    c(rep(0,8),rep(NA,8),rep(0,8)),
                                    c(rep(0,16),rep(NA,8))))

#Ver matriz
target.mat.bifac


# Usar la matriz objetivo para rotar las cargas
rotated.bifac <- funRotate(# Los resultados de EFA que se van a rotar
                 efa.bifac, 
                 # Target rotation (ortogonal)
                 fun = "targetT", 
                 # Especificar la matriz objetivo
                 Target = target.mat.bifac)
                             
# Para usar cargas como valores iniciales para el ESEM, necesitamos
# revertirlos a la métrica de los elementos. lo hacemos
# multiplicando las cargas por las desviaciones estándar de los ítems
rotated.bifac.loadings.unstd <- rotated.bifac@loading*describe(da)$sd
                              
# Manipular la matriz de carga, nombrar columnas, etiquetar factores
loadings.esem.bifac<- melt(round(rotated.bifac.loadings.unstd,3))
colnames(loadings.esem.bifac) <- c("item","latent","value")
loadings.esem.bifac$latent<-c(rep("G",24),
                            rep("F1",24),
                            rep("F2",24),
                            rep("F3",24))
                              
# Establecer anclas para el bifactor ESEM
anchors.bifac <- as.data.frame(cbind(latent = c("G","G","G",
                                                "F1","F2","F3"),
                                       item = c("A1","B1","C1",
                                                "A1","B1","C1")))

#Examinamos las sintaxis obtenida
esem.lavaan.syntax(loadings.esem.bifac,anchors.bifac)
#Estimación
fit.BIFA.ESEM <- cfa(model = c(esem.lavaan.syntax(loadings.esem.bifac,anchors.bifac),
# Adding orthogonal constraints on factors
'G ~~ 0*F1 + 0*F2 + 0*F3
F1 ~~ 0*F2 + 0*F3
F2 ~~ 0*F3'),
data =  da,
# Se puede usar estimador de minimos cuadrados con media varianza ajusta y matrices de correlación policórica
estimator = "WLSMV", ordered= names(da))
                              
                              
#Gráficos
lavaanPlot(name = "BESM", model = fit.BIFA.ESEM)
# Podemos echar un vistazo a la forma gráfica del modelo

#Path analisis
semPaths(fit.BIFA.ESEM, intercepts = FALSE,residuals=FALSE,edge.label.cex=0.7,
sizeInt=5,edge.color ="black",esize = 1.5, label.prop=1,
rotation = 1, sizeMan = 2.5,sizeLat = 8,bifactor=TRUE,
layout = "tree3", style = "lisrel",nCharNodes = 0, whatLabels = "std")

# Examinamos las cargas estandarizadas
select(arrange(arrange(parameterestimates(fit.BIFA.ESEM,standardized = T)[1:96,], rhs),lhs),
               # solo ver un subconjunto de salida de interés
               lhs:est,pvalue,std.all)

# Extrayendo índices de ajuste del modelo
fitmeasures(fit.BIFA.ESEM, fit.subset)


#Resumen
summary(fit.BIFA.ESEM, fit.measures = TRUE, standardized=T, rsquare=TRUE)

#Evaluación de los modeloa
fit.summary <- round(rbind(fitmeasures(fit.UNI.CFA, fit.subset),
                           fitmeasures(fit.FA3.CFA, fit.subset),
                           fitmeasures(fit.BIFA.CFA, fit.subset),
                           fitmeasures(fit.FA3.ESEM, fit.subset),
                           fitmeasures(fit.BIFA.ESEM, fit.subset)),3)


rownames(fit.summary) <- c("Unidimensional", "Oblicuo AFC","Bifactor AFC","Oblicuo ESEM","Bifactor ESEM")
colnames(fit.summary) <- c("χ²","p","gl","CFI","TLI","RMSEA","Lower","Upper","SRMR","WRMR")
fit.summary

#Guardar
fit.summary <- as.data.frame(fit.summary)
fit.summary
write.xlsx(fit.summary,"Modelos.xlsx",colNames=TRUE, rowNames=TRUE)

# Examine the loadings 
CargasBESEM<-select(arrange(arrange(parameterestimates(fit.BIFA.ESEM,standardized = T)[1:96,], rhs),lhs),
                    # only view a subset of output of interest
                    lhs:est,pvalue,std.all)
#GUARDAR
CargasBESEM <-as.data.frame(CargasBESEM)
CargasBESEM <- CargasBESEM[,-1]
CargasBESEM <- CargasBESEM %>% 
  mutate_if(is.numeric, round, digits = 3)
#Crear excel de resultados
write.xlsx(Std, "CargasBESEM.xlsx")

