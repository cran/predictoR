# Códigos de BAYES ---------------------------------------------------------------------------------------------------------

#Crea el modelo Bayes
bayes.modelo <- function(variable.pr = NULL){
  return(paste0("modelo.bayes <<- train.bayes(",variable.pr,"~., data = datos.aprendizaje)"))
}

#Codigo de la prediccion de Bayes
bayes.prediccion <- function() {
  return(paste0("prediccion.bayes <<- predict(modelo.bayes, datos.prueba, type = 'class')"))
}

#Codigo de la matriz de confucion de Bayes
bayes.MC <- function(){
  return(paste0("MC.bayes <<- confusion.matrix(datos.prueba, prediccion.bayes)","\n"))
}

