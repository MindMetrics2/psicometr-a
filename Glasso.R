
library(psych)
library(qgraph)
library(lavaan)


items <-c("Presto mucha atención a los sentimientos",			
"Normalmente me preocupo mucho por lo que siento",
"Normalmente dedico tiempo a pensar en mis emociones",				
"Pienso que merece la pena prestar atención a mis emociones y estado de ánimo",					
"Dejo que mis sentimientos afecten a mis pensamientos",				
"Pienso en mi estado de ánimo constantemente",			
"A menudo pienso en mis sentimientos",				
"Presto mucha atención a cómo me siento",				
"Tengo claros mis sentimientos",
"Frecuentemente puedo definir mis sentimientos",
"Casi siempre sé cómo me siento",					
"Normalmente conozco mis sentimientos sobre las personas",			
"A menudo me doy cuenta de mis sentimientos en diferentes situaciones",				
"Siempre puedo decir cómo me siento",
"A veces puedo decir cuáles son mis emociones",
"Puedo llegar a comprender mis sentimientos",
"Aunque a veces me siento triste, suelo tener una visión optimista",				
"Aunque me sienta mal, procuro pensar en cosas agradables",
"Cuando estoy triste, pienso en todos los placeres de la vida",		
"Intento tener pensamientos positivos aunque me sienta mal",			
"Si doy demasiadas vueltas a las cosas, complicándolas, trato de calmarme",			
"Me preocupo por tener un buen estado de ánimo",
"Tengo mucha energía cuando me siento feliz",			
"Cuando estoy enfadado intento cambiar mi estado de ánimo")

# Observed variables:
obsvars <- c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11",
             "I12","I13","I14","I15","I16","I17","I18","I19","I20","I21","I22",
             "I23","I24")
##----------------------------------------------------------------------------------------------------------
#Importar base de datos en da
da=ESCALA_TMMS_24

##----------------------------------------------------------------------------------------------------------
#Aislar la escala principal
de<-data.frame(cbind(da$E1,da$E2,da$E3,da$E4,da$E5,da$E6,da$E7,da$E8
                     ,da$E9,da$E10,da$E11,da$E12,da$E13,da$E14,da$E15,da$E16
                     ,da$E17,da$E18,da$E19,da$E20,da$E21,da$E22,da$E23,da$E24))

# Calcular la matriz de correlaciones
cor_matrix <- cor(de)


qgraph(corMat, edge.labels = F, esize = 10, labels = LETTERS[1:6], fade = FALSE)


view (de) # ver la base de datos “bfi” 
summary(de) # computer mínimo, máximo, rango, media, etc. de los datos “bfi” 
dim(de) #número de variables y casos de la base “bfi” 
names(de) #Nombres de las variables en la base “bfi” 
describe(de) #Estadísticos descriptivos de la base “bfi”

corMat <- cor_auto(de) 

# generar grupos de ítems que se corresponden con las cinco dimensiones, cada dimensión contiene 5 ítems
Groups <- c(rep("Atencion", 8), rep("Claridad", 8), rep("Reparacion", 8))


Graph_lasso <- qgraph(corMat, graph = "glasso", layout = "spring", threshold = TRUE , 
                      tuning = 0.25, sampleSize = nrow(de), 
                      theme = "gray", groups = Groups,
                      labels = obsvars) # estimar la red con 25 ítem y 5 dimensiones con el método GLASSO, colores grises, Figura 5 del trabajo.


Graph_lasso<- qgraph(corMat, graph = "glasso", 
                      layout = "spring", tuning = 0.25, sampleSize = nrow(de), 
                      groups = Groups, palette = "colorblind", threshold = TRUE,
                      edge.labels=TRUE,
                      esize=5, labels = obsvars,
                      curveAll=2,
                      palette="ggplot2",
                      legend.cex=0.18,
                      legend=T,
                      nodeNames=items,
                      edge.label.cex=0.5)# estimar la red con 25 ítem y 5 dimensiones con el método GLASSO, Figura 5 del trabajo en color.


centralityPlot(net2,orderBy = "Strength") # estimar los índices de centralidad, Figura 6 del trabajo
clusteringPlot(Graph_lasso)
centralityTable(Graph_lasso)
clusteringTable(Graph_lasso)

centralityPlot(Graph_lasso,orderBy = "Strength")


20 8 7 18 13 3}

13 22 5