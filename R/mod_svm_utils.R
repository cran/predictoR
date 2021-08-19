# Códigos de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm.",kernel," <- traineR::train.svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')"))
}

#Código de la predicción de svm
svm.prediccion <- function(kernel = "linear") {
  return(paste0("prediccion.svm.",kernel," <<- predict(modelo.svm.",kernel," , datos.prueba, type = 'class')"))
}

#Código de la matriz de confución de svm
svm.MC <- function( kernel = "linear"){
  return(paste0("MC.svm.",kernel," <<- confusion.matrix(datos.prueba, prediccion.svm.",kernel,")","\n"))
}

#Código del gráfico de svm
svm.plot <- function(var.pred,train,  variables, resto, kernel = "linear"){
  if(is.null(variables)){
    return("NULL")
  }

  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i]," = ", i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  s <- paste0("modelo.svm.temp <<- traineR::train.svm(",var.pred,"~",variables[1],"+",variables[2],", data = datos.aprendizaje, kernel = '",kernel,"') \n")
  color <- length(unique(train[,var.pred]))
  color <- as.string.c(gg_color_hue(color))
  return(paste0(s,"plot(modelo.svm.temp, datos, ",variables[1],"~",variables[2],", slice = ",l,", col = ",color,")"))
}
