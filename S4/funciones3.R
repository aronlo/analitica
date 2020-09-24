
subsobre_regre <- function(x,data)
{
#x=30
data$rango<-ifelse(data$y<(1-x/100)*data$y_estimada, '1',  
            ifelse(data$y<=(1+x/100)*data$y_estimada,'2',
            ifelse(data$y>(1+x/100)*data$y_estimada,'3','4.Revisar')))

data$R_ratio_uso<-ifelse(is.na(data$y),'12.Revisar',
                  ifelse(data$y<=1000,'01.<=1k',
                  ifelse(data$y<=2000,'02.<=2k',
                  ifelse(data$y<=3000,'03.<=3k',
                  ifelse(data$y<=4000,'04.<=4k',
                  ifelse(data$y<=5000,'05.<=5k',
                  ifelse(data$y<=6000,'06.<=6k',
                  ifelse(data$y<=7000,'07.<=7k',
                  ifelse(data$y<=8000,'08.<=8k',
                  ifelse(data$y<=9000,'09.<=9k','10.>10k'
                  ))))))))))


resumen<-table(data$R_ratio_uso,data$rango)
resumen<-as.data.frame(resumen)
resumen<-cast(resumen, Var1 ~ Var2, value = 'Freq')
resumen$total<-resumen$`1`+resumen$`2`+resumen$`3`
resumen$sub  <-resumen$`1`/resumen$total
resumen$coinc<-resumen$`2`/resumen$total
resumen$sobre<-resumen$`3`/resumen$total
totales<-data.frame("Totales",
                    sum(resumen$`1`),
                    sum(resumen$`2`),
                    sum(resumen$`3`),
                    sum(resumen$`1`) + sum(resumen$`2`) + sum(resumen$`3`),
                    sum(resumen$`1`) / sum(resumen$total),
                    sum(resumen$`2`) / sum(resumen$total),
                    sum(resumen$`3`) / sum(resumen$total)
                    
)

colnames(resumen)<-c("Rango  Real","Subestimacion(#)","Coincidencia(#)","Sobrestimacion(#)","TOTAL(#)","Subestimacion(%)","Coincidencia(%)","Sobrestimacion(%)")
colnames(totales)<-c("Rango  Real","Subestimacion(#)","Coincidencia(#)","Sobrestimacion(#)","TOTAL(#)","Subestimacion(%)","Coincidencia(%)","Sobrestimacion(%)")
resumen<-rbind(resumen,totales)
resumen <- data.frame(resumen)
return(resumen)
}