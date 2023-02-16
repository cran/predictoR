
#' Error Evolution
#'
#' @param model a random forest model.
#' @param label a label plot.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_rf_error
#' @import echarts4r
#' @import traineR
#' @examples
#' model <- traineR::train.randomForest(Species~., iris, mtry = 2, ntree = 20)
#' label <- "Trees"
#' e_rf_error(model, label)
#' 
#' 
e_rf_error <- function(model, label = "Trees") {
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
      x = label,
      y = 'Error') |>   
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading() 
  
  plot.rf.err$x$opts$series[[which(plot.rf.err$x$opts$legend$data == "OOB")]]$lineStyle$type <- "solid"
  plot.rf.err
}
