
## GINI y KS

calcula_indicadores <- function(objeto_logit)
{
  objeto_logit.ks <- ks.test(x = objeto_logit$fitted.values[which(objeto_logit$y == 0)],
                             y = objeto_logit$fitted.values[which(objeto_logit$y == 1)])
    ks <- objeto_logit.ks$statistic
    objeto_logit.roc <- roc(response=objeto_logit$y,predictor=objeto_logit$fitted.values)
    objeto_logit.gini <- 2*objeto_logit.roc$auc-1
    out <- data.frame(variable="logistico",Gini=round(100*objeto_logit.gini,2),KS=round(100*ks,2))
    row.names(out) <- NULL
    return(out)
  }

## Correlaciones

correlacionS <- function(m) {
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut])
}


