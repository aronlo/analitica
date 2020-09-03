getwd()
setwd('C:/Users/Aron/Desktop/Analitica/S2')
Data_wolfe <- read.csv(file="wolf_hormone_data_for_dryad.csv", header=TRUE, sep=",")
Data_wolfe.data <- data.frame(Data_wolfe)
head(Data_wolfe.data)
dim(Data_wolfe.data)

#Para ver los valores acumulados que son nulos
sum(is.na(Data_wolfe.data$Ppgmg))
sum(is.na(Data_wolfe.data["Ppgmg"]))

dat <- Data_wolfe.data

#Filtrado
nrow(dat[dat$Sex == "M",])
nrow(dat[dat$Sex == "U",])

library(ggplot2)

#Para vetr proporcion
table(dat$Sex)

prop.table(table(dat$Sex))

max(dat$Ppgmg, na.rm= "TRUE")

machos <- subset(dat, (dat$Sex == "M"))

mean(machos$Cpgmg)