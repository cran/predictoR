#Gráfico de evolucion del error
e_evol_error <- function(x, label = "Iterations") {
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
      x = label,
      y = "Error"
    )|>   e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()
}


