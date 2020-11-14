
rm(list=ls())

#########################################################################
### -- ANALÍTICA PREDICTIVA DE DATOS -- ## 
#########################################################################


source("path_to/ins_paquetes.R")

ins_paquetes('rminer','lattice','sqldf','DMwR','caret','C50','randomForest')


########### 1) LIBRERIAS A UTILIZAR ################# 
library(stringr)
library(purrr) #A complete and consistent functional programming toolkit for R.
library(dplyr)
library(MLmetrics)
library(party)
library(rminer)
library(lattice)
library(mlr)
library(sqldf)
library(ggplot2)
library(DMwR)
library(caret)
library(C50)
library(rpart)
library(randomForest)
library(writexl)
library(readxl)
########### 2) DATA A UTILIZAR ################# 
setwd(" ")

train<-read_excel("Potenciales_clientes.xlsx", sheet = "Sheet1 (2)")

########### 3) TRATAMIENTO DE LA DATA ################# 

## en primer lugar ver el analisis descriptivo de la data

resumen <- data.frame(summarizeColumns(train))

########### 4) PRIMER ANALISIS  ################# 

## generamos una copia de la data original

data_train <- train

##### pgta PARTE B a


m<-c("Medio*", "Medio_")
zeta<- data_train$Tipo
r<-zeta[map_lgl(zeta, ~ !any(str_detect(., m)))]
main_data <- data_train[ data_train$Tipo %in% r, ]



## imputar en primer lugar la data

data_train <- mlr::impute(main_data, classes = list(factor = imputeMode(), 
                                                  integer = imputeMode(),
                                                  numeric = imputeMedian()))
data_train <- data_train$data[,1:ncol(main_data)]


## luego dar un orden a las variables

summary(data_train)
cuantis <- data_train %>% select(Age,Credit_amount,Duration)
cualis <- data_train %>% select(-Age,-Credit_amount,-Duration)

data_train <- cbind(cuantis,cualis)
str(data_train)
# recodificacion manual
data_train$Sex <- ifelse(data_train$Sex=='male',2,1)

###COMPLETE#############



# categorizando a factor

data_train[,(ncol(cuantis)+1):ncol(data_train)] <- lapply(data_train[,(ncol(cuantis)+1):ncol(data_train)],as.factor)
#lapply : Apply a Function over a List or Vector
summary(data_train)

data_train<-na.omit(data_train)
summary(data_train)
######### 5) PARTICION MUESTRAL  #################

## Particionando la Data

set.seed(1234)
sample <- createDataPartition(data_train$Tipo, p = .60,list = FALSE,times = 1)

data.train <- data_train[ sample,]
data.test <- data_train[-sample,]

######### 6) MODELADO #################

# modelo 1.- Random forest

modelo1 <- 
  
  
  
  
#mtry	
#Number of variables randomly sampled as candidates at each split.

# probabilidades
proba1 <- predict(modelo1, newdata=data.test,type="prob")
proba1 <- proba1[,2]

# indicadores para probabilidades
GINI_1     <- Gini(proba1,   as.numeric(as.character(data.test$Tipo)))
ks_1       <- KS_Stat(proba1,as.numeric(as.character(data.test$Tipo)))
LogLoss_1  <- LogLoss(proba1,as.numeric(as.character(data.test$Tipo)))

GINI_1 
ks_1 
LogLoss_1 
# Calcular los valores predichos
PRED <- predict(modelo1, newdata=data.test,type="response")

# Calcular la matriz de confusion

### COMPLETE

tabla
