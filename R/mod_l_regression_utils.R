# Códigos de  RL --------------------------------------------------------------------------------------------------------------

#Crea el modelo RL
rl.modelo <- function(variable.predecir = NULL){
  return(paste0("modelo.rl <<- train.glm(",variable.predecir,"~., data = datos.aprendizaje, family = binomial)"))
}

#Código de la prediccion de rl
rl.prediccion <- function() {
  return(paste0("prediccion.rl <<- predict(modelo.rl, datos.prueba, type = 'class')"))
}

#Código de la matriz de confucion de rl
rl.MC <- function(){
  return(paste0("MC.rl <<- confusion.matrix(datos.prueba, prediccion.rl)","\n"))
}

