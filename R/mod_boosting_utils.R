# Códigos de BOOSTING --------------------------------------------------------------------------------------------------------

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, minsplit = 1){
  iter     <- ifelse(!is.numeric(iter), 50, iter)
  maxdepth <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo   <- paste0("modelo.boosting <<- train.adabag(",variable.pr,"~., data = datos.aprendizaje, mfinal = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

#Código de la prediccion de boosting
boosting.prediccion <- function() {
  return(paste0("prediccion.boosting <<- predict(modelo.boosting, datos.prueba, type = 'class')"))
}


#Código de la matríz de confución de boosting
boosting.MC <- function(){
  return(paste0("MC.boosting <<- confusion.matrix(datos.prueba, prediccion.boosting)\n"))
}

#Código del grafico de boosting
boosting.plot <- function(){
  return(paste0("error(modelo.boosting, datos.aprendizaje) -> evol.train\n",
                 "e_evol_error(evol.train)"))
}

#Código del grafico de importancia de variables
boosting.plot.import <- function() {
  return(paste0(
    "aux <- data.frame(importancia = modelo.boosting$importance)\n",
    "aux$nombre <- row.names(aux)\n",
    "aux$importancia <- abs(aux$importancia)\n",
    "aux <- aux[order(aux$importancia, decreasing = T), ]\n\n",
    "aux |>  e_charts(nombre) |>  e_bar(importancia, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#Código del grafico de evolucion del error
e_evol_error <- function(x) {
  if (!((class(x) %in% c("errorevol")))) 
    stop("x class should be errorevol")
  train    <- x$error
  evolplot <- data.frame(x = c(1:length(x$error)), train = train)
  evolplot |> 
    e_charts(x) |> 
    e_line(train) |> 
    e_title("Ensemble error vs number or trees",
            left = 'center',
            top = 5,
            textStyle = list(fontSize = 15))|> 
    e_legend(orient = 'vertical',
             right = '20', top = '10%') |> 
    e_axis_labels(
      x = "Iterations",
      y = "Error"
    )|>   e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()
}

#Reglas de boosting
rules.boosting <- function(i){
  return(paste0("rules(modelo.boosting$trees[[",i,"]])"))
}

