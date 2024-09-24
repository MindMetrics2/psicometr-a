library(apaTables)
library(corrr)

#Renombrar la base de datos
da=ESCALA_TMMS_24

#Inspeccionar variables del dataframe
names(da)

#Cortar la base de datos o dataframe
df<-dplyr::select (da,TMMS24,BULLYING)

##----------------------------------------------------------------------------------------------------------
#Colocar la ruta donde se exportar?n las hojas de c?lculo 
setwd('F:/Psicologia/Analisis en R/Ejemplo')

#crear un objeto que sea la propia matriz de correlaciones
correlac<-cor(df)
View(correlac)

mvn(de, univariateTest = "SW")
#crear directaente la tabla de correlaciones en APA en formato word
apa.cor.table(df, filename = "correlaciÓnapa.doc", table.number = 2,show.conf.interval = FALSE, landscape = TRUE)

#crar figura de correlaciones con histograma, diagrama de puntos.
pairs.panels(df, pch=20,stars=TRUE,main="Correlaciones")

#red de correlaciones
df%>% correlate(method="pearson") %>% 
  network_plot()
