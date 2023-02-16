# Divide un string con "-"
split_name <-function(name, idioma){
  nom.aux <- unlist(strsplit(name, "-"))
  ifelse(length(nom.aux) == 1,
         tr(nom.aux, idioma),
         paste0(tr(nom.aux[1], idioma),"-",nom.aux[2]))
}


indices.comp <- function(category, MCs, n){
  ind.categ  <- vector(mode = "list",   length = length(category))
  names(ind.categ) <- category
  
  for (cat in category) {
    ind.categ[[cat]] <- vector(mode = "numeric",   length = n)
  }
  col_      <- gg_color_hue(n)
  rep       <- vector(mode = "numeric",   length = n)
  value     <- vector(mode = "numeric",   length = n)
  color     <- vector(mode = "character", length = n)
  
  for (i in 1:n){
    rep[i]     <- i
    color[i]     <- col_[i]
    value[i]     <- precision.global(MCs[[i]])
    
    for (cat in category) {
      ind.categ[[cat]][i] <- sapply(MCs[i], 
                                    precision(cat))
    }
  }
  
  grafico    <- data.frame(rep, value, color)
  
  resultados <- list(grafico = grafico, global = value, categories = ind.categ)
  return(list(grafico = grafico, global = value, categories = ind.categ))
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


# Gráfico comparativo

comp.lineas <- function(datos.grafico, labels = c("Global", "repeticion"), error = FALSE) {
  
  comp_plot <- datos.grafico |>
    e_charts(rep) |>
    e_line(value, name = var) |>  
    e_y_axis(formatter = e_axis_formatter("percent",
                                          digits = 0)) |>
    e_axis_labels(x = labels[2],
                  y = paste('%', labels[1])) |>
    e_title(labels[1],
            left = "center",
            top = 5,
            textStyle = list(fontSize = 20)) |>
    e_tooltip(formatter = e_JS("function(params){
                                           return('<strong>' + params.value[0] + ' </strong>' +",
                               " parseFloat(params.value[1] * 100).toFixed(2) + '%' )}")) |>
    e_datazoom(show = F,startValue=1) |>
    e_legend(show = T, type = "scroll", bottom = 1) |>
    e_show_loading()|> e_x_axis(nameLocation = 'middle', nameGap = 35)
  comp_plot
}







