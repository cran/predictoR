# Divide un string con "-"
split_name <-function(name, idioma){
  nom.aux <- unlist(strsplit(name, "-"))
  ifelse(length(nom.aux) == 1,
         tr(nom.aux, idioma),
         paste0(tr(nom.aux[1], idioma),"-",nom.aux[2]))
}

# Obtiene la cantidad de categorías de la variable a predecir
num.categorias.pred <- function(test, var.pred){
  return(length(levels(test[,var.pred])))
}
# Obtiene los puntos para graficar la curva ROC
roc.values <- function(score, clase, n = 20) {
  res <- lapply(seq(1, 0, length = n), function(umbral) {
                FN <- length(which(score[clase == levels(clase)[2]] < umbral))
                TP <- length(which(score[clase == levels(clase)[2]] >= umbral))
                FP <- length(which(score[clase == levels(clase)[1]] >= umbral))
                TN <- length(which(score[clase == levels(clase)[1]] < umbral))
                
                c(TP / ( FN + TP ), TN / ( FP + TN ))})
  res <- append(list(c(0,1)), res)
  res[!duplicated(res)]
}












