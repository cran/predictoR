# Pagina de DT --------------------------------------------------------------------------------------------------------------

#Crea el modelo DT
dt.modelo  <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo   <- paste0("modelo.dt.",split," <<- train.rpart(",variable.pr,"~., data = datos.aprendizaje,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))")
  return(codigo)
}

#Código de la predicción de DT
dt.prediccion <- function(tipo) {
  return(paste0("prediccion.dt.",tipo," <<- predict(modelo.dt.",tipo,", datos.prueba, type='class')"))
}

#Código de la matriz de confución de dt
dt.MC <- function(tipo){
  return(paste0("MC.dt.",tipo," <<- confusion.matrix(datos.prueba, prediccion.dt.",tipo,")","\n"))
}

#Código del gráfico de dt
dt.plot <- function(tipo, num = 1){
  #∫num <- length(levels(datos[,var.pred]))
  return(paste0("prp(modelo.dt.",tipo,", type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
                fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
                box.col = gg_color_hue(",num,")[modelo.dt.",tipo,"$frame$yval])"))
}
