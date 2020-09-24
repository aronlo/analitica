#########################################################################
### -- ANAL?TICA PREDICTIVA DE DATOS -- ## 
#########################################################################

#########################################################################

#setwd("C:/Users/Intel/Documents/CURSO_ANALITICA_PREDICTIVA/CIS/2020-2B/semana4/REGRESION_RSTUDIO/Semana 4/codigo")
setwd("/cloud/project/S4")


########### 1) LIBRERIAS A UTILIZAR ################# 

#source("C:/Users/Intel/Documents/CURSO_ANALITICA_PREDICTIVA/CIS/2020-2B/semana4/REGRESION_RSTUDIO/Semana 4/codigo/ins_paquetes.R")
source("/cloud/project/S4/ins_paquetes.R")

ins_paquetes('MLmetrics','reshape','data.table','dplyr',
             'party','lattice','sqldf','ggplot2','mlr',
             'nortest','tseries')

library(reshape)
library(data.table)
library(dplyr)
library(MLmetrics)
library(party)
library(lattice)
library(sqldf)
library(ggplot2)
library(mlr)
library(nortest)
library(tseries)

########### 2) DATA A UTILIZAR ################# 

train <- read.csv("dataset_taller.csv")

########### 3) TRATAMIENTO DE LA DATA ################# 

## en primer lugar ver el analisis descriptivo de la data

#Se hace un resumen del dataset.
resumen <- data.frame(summarizeColumns(train))
#Se crea un csv con el resumen.
write.csv(resumen,"tabla_resumen_taller.csv")

########### 4) PRIMER ANALISIS  ################# 

## generamos una copia de la data original

data_train <- train

## imputar en primer lugar la data
#Todos los registros vacios uno decide el tipo de imputación a hacer.
#Se usa MLR
#Impute and re-impute data
#Allows imputation of missing feature values through various techniques. 

data_train <- mlr::impute(train, classes = list(factor = imputeMode(), 
                                                integer = imputeMode(),
                                                numeric = imputeMedian()))
#Se asigna como entrenamiento
data_train <- data_train$data[,1:ncol(train)]


## luego dar un orden a las variables

summary(data_train)

#dplyr::reexports	%>%	Objects exported from other packages. Pipe operator
#Se toman las variables nominales (filtrar data set)
cualis <- data_train %>% select(CHAS)
#Se toman las variables cualitativas (filtrar data set)
cuantis <- data_train %>% select(-CHAS)


#Unir ambos en uno solo
data_train <- cbind(cualis,cuantis)
data_train

summary(data_train)
# recodificacion manual

## ojo: siempre y cuando aplique realizarlo

## ver correlacion antes de categorizar

source("/cloud/project/S4/funciones.R")

#Se genera una talba de correlación mediante spearman
corre <- cor(data_train,method = c("spearman"))

summary(corre)

## colocamos la primera funcion de correlacion. Se le dará mayor prioridad a los correelacionados
corre <- correlacionS(corre)
#Se filtran aquellos con mayor valor 0.6
corre$filtro <- ifelse(abs(corre$cor)>0.6,1,0)

#Se hace un csv
write.csv(corre,"correlacion_variables_taller.csv",row.names = F)

# categorizando a factor. Convertiremos a factores, ya no son cualitativas.
data_train[,1:ncol(cualis)] <- lapply(data_train[,1:ncol(cualis)],as.factor)
summary(data_train)

## primer modelo regresion multiple ##

# retiramos la variable correlacionada
data.frame(names(data_train))

summary(data_train)

formula <-    MEDV ~ CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT 


modelo1 <- glm(formula,data=data_train,gaussian(link = "identity"))

# indicadores mediante la segunda funcion
calcula_indicadores(modelo1)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo1)

# ver la significancia del modelo
summary(modelo1)

## reajustando

formula <- MEDV ~ CRIM + ZN + NOX + RM + DIS + RAD + TAX + PTRATIO + B + LSTAT

modelo2 <- glm(formula,data=data_train,gaussian(link = "identity"))

# indicadores mediante la segunda funcion
calcula_indicadores(modelo2)

# pesos del modelo importancia mediante la tercera funcion
pesos(modelo2)

# ver la significancia del modelo
summary(modelo2)

# Obtenemos los valores ajustados o predichos
estimados <- modelo2$fitted.values

ggplot(data = data_train, aes(x = LSTAT, y = MEDV)) + geom_point(color = "red") +
  geom_line(aes(y = estimados), color = "blue") +
  geom_segment(aes(x = LSTAT, xend = LSTAT, y = MEDV, yend = estimados, color="Distancia"), color = "grey80") +
  labs(xlab = "LSTAT", ylab = "MEDV") + 
  theme_bw()

plot(modelo2$residuals)

plot(modelo2$fitted.values,modelo2$residuals)

## Test de Shapiro-Wilk
shapiro.test(x = modelo2$residuals)
## Test de Kolmogorov-Smirnov. Prueba no paramétrica
ks.test(x = modelo2$residuals,"pnorm")
## modificaci?n de Lillefors
lillie.test(x = modelo2$residuals)
## Test de Jarque-Bera
jarque.bera.test(x = modelo2$residuals)