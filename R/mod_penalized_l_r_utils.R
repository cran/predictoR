
select_beta <- function(modelo, lambda) {
  menor   <- 0
  mayor   <- 0
  cercano <- 0
  for (n in modelo$lambda) {
    if(n == exp(lambda)){
      cercano <- n
    }else if (n < exp(lambda)){
      menor <- n
    }else if (n > exp(lambda)){
      mayor <- n
    }
  }
  if((mayor - exp(lambda)) < (exp(lambda) - menor)){
    cercano <- mayor
  }else{
    cercano <- menor
  }
  return(which(modelo$lambda == cercano))
}

select_landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE, type = "ridge"){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",type," <<- glmnet::cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')\n",
         "e_posib_lambda(cv.glm.",type,")")
}

coeff.values <- function(data, name ){
  lapply(1:length(data[["x"]]), function(i) c(data[["x"]][[i]], data[[name]][[i]]))
}

#' Possible lambda
#' 
#' @param cv.glm a cv.glmnet model.
#' @param labels a character vector of length 3 specifying the titles to use on legend.
#' 
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_posib_lambda
#' @import echarts4r
#' @importFrom glmnet cv.glmnet
#' @examples
#' x         <- model.matrix(Species~., iris)[, -1]
#' y         <- iris[,'Species']
#' cv.glm    <- glmnet::cv.glmnet(x, y, standardize = TRUE, alpha = 1, family = 'multinomial')
#' e_posib_lambda(cv.glm)
#' 
#' 
e_posib_lambda <- function(cv.glm, labels = c("Valor Superior", "Valor Inferior", "lambda")){
  x  <- log(cv.glm$lambda)
  y  <- cv.glm$cvm
  x1 <- x[cv.glm$index[[1]]]
  x2 <- x[cv.glm$index[[2]]]
  upper <- cv.glm$cvup
  lower <- cv.glm$cvlo
  name  <- cv.glm$name[[1]]
  data.lambda <- data.frame(x, y, upper, lower, name)
  plot  <- data.lambda |> 
    e_charts(x) |> 
    e_scatter(y, symbol_size = 7) |> 
    e_error_bar(lower, upper, 
                tooltip = list(formatter = e_JS(paste0("function(params){",
                                                       "return('<b>", labels[1], ": </b>' + ",
                                                       "Number.parseFloat(params.value[2]).toFixed(3) + ",
                                                       "'<br/><b>", labels[2], ": </b>' + ",
                                                       "Number.parseFloat(params.value[1]).toFixed(3))}")))) |> 
    e_mark_line(data = list(xAxis = x1,
                tooltip = list(formatter = e_JS(paste0("function(params){",
                                                        "return('<b>Log(lambda.min): </b>' + ",
                                                        "Number.parseFloat(params.value).toFixed(4))}"))))) |> 
    e_mark_line(data = list(xAxis = x2,
                tooltip = list(formatter = e_JS(paste0("function(params){",
                                                       "return('<b>Log(lambda.1se): </b>' + ",
                                                       "Number.parseFloat(params.value).toFixed(4))}"))))) |> 
    e_axis_labels(
      x = labels[3],
      y = name)|> 
    e_x_axis(
      formatter = e_axis_formatter(digits = 1))  |>  
    e_legend(FALSE) |>  
    e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()
  plot$x$opts$xAxis[[1]]$type <- "value"
  plot
}

#' Coefficients and lambda
#' 
#' @description Plot the coefficients and selected lambda of a glmnet model.
#'
#' @param model a glmnet model.
#' @param category a category of the variable to be predicted.
#' @param sel.lambda the selected lambda.
#' @param label a character specifying the title to use on selected lambda tooltip.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @import traineR
#' 
#' @export e_coeff_landa
#'
#' @examples
#' modelo <- traineR::train.glmnet(Species~., iris)
#' e_coeff_landa(modelo, 'setosa', log(modelo$lambda[1]))
#' 
e_coeff_landa <- function(model, category, sel.lambda = NULL, label = 'Log Lambda') {
  data       <- data.frame(t(as.data.frame(as.matrix(model$beta[[category]]))))
  x          <- round(log(model$lambda), 5)
  data       <- cbind(x = x, data)
  data       <- data[order(data$x),]
  new        <- list()
  for (i in 1:length(colnames(data)[-1])) {
    nom      <- colnames(data)[i+1]
    new[[i]] <- list(
      type     = "line", 
      data     = coeff.values(data, nom), 
      name     = nom, 
      label    = list(show      = TRUE, 
                      position  = 'left',
                      formatter = e_JS(paste0("function(params){
                                              if(params.dataIndex == 0){
                                                return('",nom,"')
                                              }else{return('')}}")))
    )
  }
  opts <- list(xAxis = list(show = TRUE, 
                            type = "value"),
               yAxis = list(show = TRUE, 
                            type = "value"),
               series = new)
  coeff_plot <- e_charts() |> 
    e_list(opts) |>
    e_axis_labels(x = label,
                  y = paste0('Coefficients: Response ', category)) |> 
    e_tooltip() |>  
    e_datazoom(show = F) |> 
    e_show_loading()|> 
    e_legend(show = FALSE)
  if(!is.null(sel.lambda)){
    coeff_plot <- coeff_plot |> 
      e_mark_line(data = list(xAxis   = sel.lambda,
                              tooltip = list(formatter = e_JS(paste0("function(params){",
                                                                     "return('<b>",label,": </b>' + ",
                                                                     "Number.parseFloat(params.value).toFixed(4))}")))))
  }
  coeff_plot
}
