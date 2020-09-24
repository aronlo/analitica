library(ggplot2)
library(tsapp)
library(forecast)
library(scales)
library(stats)
library(TTR)

#Carga de datos
df <- read.csv("./datasets/Lumini_dataset.csv",header=TRUE, sep=";")
serie1 <- ts(df$Ventas)
timeseries <- ts(serie1, frequency=12, start=c(1983,1))

timeseries <- ts(serie1, frequency=12, start=c(1983,1))
timeseries
timeseriescomponents <- decompose(timeseries)
plot(timeseriescomponents)



timeseries2components <- decompose(timeseries,type="multiplicative")


tendencia <- data.frame(timeseries2components$trend)

tendencia$timeseries2components.trend[is.na(tendencia$Yt.ts.desc.trend)]=0

tendencia$x <- seq(1:nrow(tendencia))


modelo <- lm(timeseries2components$trend ~x, data= tendencia)
tendencia_estimada <- modelo$fitted.values

estacional <- data.frame(timeseries2components$seasonal)



dataf <- data.frame(tendencia_estimada,estacional)
colnames(dataf) <- c('tend_est','estacionalidad')

dataf$Yt_est <- dataf$tend_est*dataf$estacionalidad

plot(timeseries2components)


