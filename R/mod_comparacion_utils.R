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


#' Create Cut-Off Probability values.
#'
#' @param Score a data.frame object.
#' @param Class the column name to apply disjunctive code.
#' @param levels the column name to apply disjunctive code.
#' @param category a character value specifying the name of the category to apply the Cut-Off Probability.
#' @param step the step for the Cut-Off Probability.
#'
#' @author Joseline Quirós <joseline.quiros@promidat.com>
#' @export prob.values
#' 
#' 
#' 
prob.values <- function(Score, Class, levels, category, step = -0.05){
  positive  <- levels[which(levels == category)]
  negative  <- levels[which(levels != category)]
  for(Corte in seq(1, 0, by = step)) {
    Prediccion <- ifelse(Score >= Corte, positive, negative)
    MC         <- table(Class, Pred = factor(Prediccion, levels = levels))
    cat("\nCorte usado para la Probabilidad = ")
    cat(Corte)
    cat("\n")
    print(general.indexes(mc = MC))
    cat("\n========================================")
  } 
}

# Obtiene la prediccioón y MC con la probabilidad y categoría seleccionada
prob.values.ind <- function(Score, Class, levels, category, Corte = 0.5, print = TRUE){
  positive  <- levels[which(levels == category)]
  negative  <- levels[which(levels != category)]
    Prediccion <- ifelse(Score >= Corte, positive, negative)
    MC         <- table(Class, Pred = factor(Prediccion, levels = levels))
    if(print){
      cat("\n========================================")
      cat("\nCorte usado para la Probabilidad = ")
      cat(Corte)
      cat("\n")
      print(general.indexes(mc = MC))
      cat("\n========================================")
    }
    return(list(MC = MC, Prediccion = Prediccion))
}

