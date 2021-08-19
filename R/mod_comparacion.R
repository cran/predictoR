#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id){
  ns <- NS(id)

  title_comp <- fluidRow(shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                         tags$div(class="multiple-select-var",
                                  selectInput(inputId = ns("roc.sel"),label = NULL,
                                              choices =  "", width = "100%")))
  tagList(
    tabBoxPrmdt(id = "BoxCom", title = title_comp,
                tabPanel(title = labelInput("tablaComp"),
                         withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("rocCurva"), 
                         withLoader(echarts4rOutput(ns('plot_roc'), height = "70vh"), 
                                    type = "html", loader = "loader4")))
  )
}
    
#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  # Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    choices      <- as.character(unique(datos[, variable]))
    if(length(choices) == 2){
      updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    }else{
      updateSelectInput(session, "roc.sel", choices = "")
    }
  })
  
  # Update Comparison Table
  output$TablaComp <- DT::renderDataTable({
    res      <- data.frame()
    idioma   <- updateData$idioma
    category <- input$roc.sel
    isolate(test <- updateData$datos.prueba)
    isolate(var  <- updateData$variable.predecir)
    tryCatch({
    for (nom in names(modelos)) {
      if(!is.null(modelos[[nom]])){
          for (alg in modelos[[nom]]) {
            ind <- general.indexes(mc = alg$mc)
            new <- data.frame(
              OAccuracy = ind$overall.accuracy,
              EAccuracy = ind$overall.error
            )
            
            for (cat in names(ind$category.accuracy)) {
              new[[cat]] <- ind$category.accuracy[[cat]]
            }
            if(length(ind$category.accuracy) ==2){
              if(!startsWith(alg$nombre, "rlr")){
                new$roc <- ROC.area(alg$prob$prediction[,category], test[,var])
              }else{
                new$roc <- ROC.area(alg$prob$prediction[,category,], test[,var])
              }
            }else{
              new$roc <- NULL
            }
            row.names(new) <- split_name(alg$nombre, idioma)
            res            <- rbind(res, new)
          } 
      }
      
    }
    colnames(res)[1]           <- tr('precG', idioma)
    colnames(res)[2]           <- tr('errG', idioma)

    if(length(ind$category.accuracy) ==2){
      colnames(res)[dim(res)[2]] <- tr('aROC', idioma)
    }
    
    res[]                      <- lapply(res, as.numeric)
    res                        <- round(res, 5)*100
    DT::datatable(res, selection = "none", editable = FALSE,
                                   options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    }, error = function(e) {
      DT::datatable(data.frame(), selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    })
  },server = FALSE)
  
  # Update Plot ROC
    output$plot_roc <- renderEcharts4r({
      idioma        <- updateData$idioma
      category      <- input$roc.sel
      mdls          <- modelos
      isolate(test  <- updateData$datos.prueba)
      isolate(var   <- updateData$variable.predecir)
      tryCatch({
      if(!is.null(test) & length(levels(test[,var])) == 2) {
        res    <- list(list(type = "line", color = "#5F5C5C" , data = list(c(1,0), c(0,1))))
        n2     <- 0
        i      <- 2
        for (nom in names(mdls)) {
          if(!is.null(mdls[[nom]])){
            for (alg in mdls[[nom]]) {
              n2 <- n2 + 1
            } 
          }
        }   
        colores    <- gg_color_hue(n2)
        for (nom in names(mdls)) {
          if(!is.null(mdls[[nom]])){
            for (alg in mdls[[nom]]) {
              if(!startsWith(alg$nombre, "rlr")){
                roc.data <- roc.values( alg$prob$prediction[,category], test[,var])
              }else{
                roc.data <- roc.values(alg$prob$prediction[,category,], test[,var])
              }
              res[[i]] <- list(type  = "line", 
                               data  = roc.data,  
                               color = colores[i - 1],
                               name  = split_name(alg$nombre, idioma),
                               tooltip = list(formatter = e_JS(paste0("function(params){",
                                                                      "return('<b>", split_name(alg$nombre, idioma), ": </b><br/>' +",
                                                                      "'<b>X: </b>' +",
                                                                      "Number.parseFloat(params.value[0]).toFixed(4) +", 
                                                                      "'<br/><b>Y: </b>' +",
                                                                      "Number.parseFloat(params.value[1]).toFixed(4))}"))))
              i <- i + 1
            } 
          }
        }
        opts <- list(
          xAxis = list(show = TRUE, inverse = TRUE),
          yAxis = list(show = TRUE),
          series = res)
        
        comp_plot <- e_charts() |>  
                          e_list(opts) |>  
                          e_legend(type = "scroll", bottom = 1) |>  
                          e_datazoom(show = F) |>  
                          e_tooltip() |>  
                          e_show_loading()
        comp_plot
      } else {
        showNotification(tr("RocNo", idioma), duration = 15, type = "warning")
        return(NULL)
      }}, error = function(e) {
        showNotification(e, duration = 15, type = "error")
        return(NULL)
      })
    })
}

## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
