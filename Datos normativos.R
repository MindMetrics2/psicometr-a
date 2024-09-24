
library(car)


#si queremos hacer baremos por sexo
Hombres <- subset(da, Sexo=='1')
Mujeres <- da[da$Sexo=='2',]

#los propios baremos

score <- da$Fobia               # Total score 
tosc <- sort(unique(da$Fobia))              # Levels of total score 
perc <- 100 * (cumsum(prop.table(table(score)))) # Percentiles 
sura <- 100 * (tosc / max(score))        # Success rate 
zsco <- sort(unique(scale(score)))       # Z-score 
tsco <- 50 + 10 * zsco                   # T-score
baremos<-cbind(tosc,perc,tsco,sura,zsco)
baremos<-as.data.frame(baremos)
baremos
write.xlsx(baremos, "baremos.xlsx")
