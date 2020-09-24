###  cuantissss

#detach("package:partykit", unload = TRUE)

data2 <- data_train_2

#### -- OPERACION -- ####
n1 <- ncol(data2)
tabla_ks_gini <- data.frame(matrix(0,nrow = (n1-1),ncol =3 )) #El valor de n a tomar en cuenta
names(tabla_ks_gini) <- paste(c("variable","MSE","casos"))
n = ncol(data2)

#dev.off()

for(i in 1:(n-1))
{
  datos = data.frame(data2[,i],data2[,n1]) ; colnames(datos)=c(names(data2[i]),names(data2[n1]))
  n = nrow(datos)
  #datos[1] <- lapply(datos[1], as.numeric)
  cero.na=function(x){ifelse(is.na(x),0,x)}
  
  pdf(paste0("",i,"_",names(data2[i]),".pdf","") , width=20, height=10)
  
  #Arboles ----
  datos.filt = datos[complete.cases(datos[,c(1)]),]
  n = nrow(datos.filt)
  datos.tree<-ctree( datos.filt[,c(2)] ~ datos.filt[,c(1)]
                     ,data=datos.filt,
                     controls=ctree_control(mincriterion=0.95, minbucket = 0.05*n))
  
  #Estimacion
  #yprob<-sapply(predict(datos.tree,type="prob"),'[[',2)
  yprob <- predict(datos.tree)
  correlac = MSE(yprob, datos.filt[,c(2)])
  ##arbol
  plot(datos.tree,main=paste(names(data2[i])," ","MSE:", round(100*(as.numeric(correlac)),2), " " ,"casos :" , n,  sep=" "), cex=0.5,type="simple")
  
  tabla_ks_gini[i,1] =paste0("",names(data2[i]),"")
  tabla_ks_gini[i,2] =round(100*(as.numeric(correlac)),4)
  tabla_ks_gini[i,3] = n
  
  dev.off()
  
}
