# Códigos de RF--------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree   <- ifelse(!is.numeric(ntree), 500, ntree)
  Codigo  <- paste0("modelo.rf <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                    " ntree =",ntree,",mtry =",mtry,")")
  return(Codigo)
}

#Código de la predicción de rf
rf.prediccion <- function() {
  return(paste0("prediccion.rf <<- predict(modelo.rf, datos.prueba, type = 'class')"))
}

#Código de la matriz de confución de rf
rf.MC <- function(){
  return(paste0("MC.rf <<- confusion.matrix(datos.prueba, prediccion.rf)\n"))
}

#Código del gráfico de importancia de variables
rf.importance.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux$MeanDecreaseAccuracy <- abs(aux$MeanDecreaseAccuracy)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$label <- row.names(aux)\n\n",
    "aux |>  e_charts(label) |>  e_bar(MeanDecreaseAccuracy, name = var) |>  \n",
    "  e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() |>  \n",
    "  e_flip_coords() |> \n",
    "  e_y_axis(inverse = TRUE) \n"
  ))
}

#Código del gráfico de error del modelo
plot.rf.error <- function(){
  return(paste0("e_rf_error(modelo.rf)\n"))
}


#' Error Evolution
#'
#' @param model a random forest model.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_rf_error
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.randomForest(Species~., iris, mtry = 2, ntree = 20)
#' e_rf_error(model)
#' 
#' 
e_rf_error <- function(model) {
  data <- data.frame(x = c(1:length(model$err.rate[,1])),cbind(model$err.rate))
  new  <- data.frame()
  for (nom in colnames(data)[-1]) {
    x      <- data[["x"]]
    y      <- data[[nom]]
    nombre <- nom
    new.   <- data.frame(x = x, y = y, nombre = nombre)
    new    <- rbind(new, new.)
  }
  
  plot.rf.err <- new |> 
    group_by(nombre) |> 
    e_charts(x) |> 
    e_line(y, lineStyle = list(type = 'dashed')) |> 
    e_legend(orient = 'vertical',
             right = '20', top = '10%') |>  
    e_axis_labels(
      x = 'Trees',
      y = 'Error') |>   
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() 
  
  plot.rf.err$x$opts$series[[which(plot.rf.err$x$opts$legend$data == "OOB")]]$lineStyle$type <- "solid"
  plot.rf.err
}
