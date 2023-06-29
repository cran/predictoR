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

  title_comp <- div(conditionalPanel("input['comparacion_ui_1-BoxCom'] != 'tabModelosComp'",
        div(shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                         tags$div(class="multiple-select-var",
                                  selectInput(inputId = ns("roc.sel"),label = NULL,
                                              choices =  "", width = "100%")))))
  tagList(
    tabBoxPrmdt(id = "BoxCom", title = title_comp,
                tabPanel(title = labelInput("tablaComp"), value = "tabtablaComp",
                         withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("rocCurva"), value = "tabrocCurva", 
                         withLoader(echarts4rOutput(ns('plot_roc'), height = "70vh"), 
                                    type = "html", loader = "loader4")))
  )
}

#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  # Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    choices      <- as.character(unique(datos[, variable]))
    if(length(choices) == 2){
      updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1]) # Actualiza las opciones de la curva ROC
    }else{
      updateSelectInput(session, "roc.sel", choices = "")
    }
  })
  
  
  # Update Comparison Table
  output$TablaComp <- DT::renderDataTable({
    res      <- data.frame() # DF de respuesta
    idioma   <- codedioma$idioma
    category <- input$roc.sel
    isolate(test <- updateData$datos.prueba)
    isolate(var  <- updateData$variable.predecir)
    tryCatch({
    for (nom in names(modelos)) { # Recorre todos los modelos 
      if(!is.null(modelos[[nom]])){
          for (alg in modelos[[nom]]) { # Recorre cada algoritmo de los modelos
            ind <- general.indexes(mc = alg$mc)
            new <- data.frame(
              OAccuracy = ind$overall.accuracy,
              EAccuracy = ind$overall.error
            ) #Precisión y error global
            
            for (cat in names(ind$category.accuracy)) { # Recorre cada categoría de la variable a predecir
              new[[cat]] <- ind$category.accuracy[[cat]] # Guarda la precisión de la categoría
            }
            if(length(ind$category.accuracy) ==2){ # Solo para 2 categorías
              if(!startsWith(alg$nombre, "rlr")){
                new$roc <- ROC.area(alg$prob$prediction[,category], test[,var]) # Calcula el area bajo la curva
              }else{
                new$roc <- ROC.area(alg$prob$prediction[,category,], test[,var])# Calcula el area bajo la curva para RLR
              }
            }else{
              new$roc <- NULL
            }
            row.names(new) <- split_name(alg$nombre, idioma) # Obtiene el nombre del modelo y algoritmo
            res            <- rbind(res, new)
          } 
      }
    }
    colnames(res)[1]           <- tr('precG', idioma) # Columna PGlobal
    colnames(res)[2]           <- tr('errG', idioma) # Columna EGlobal

    if(length(ind$category.accuracy) ==2){
      colnames(res)[dim(res)[2]] <- tr('aROC', idioma)# Columna Area ROC
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
      idioma        <- codedioma$idioma
      category      <- input$roc.sel # Categoría seleccionada
      mdls          <- modelos
      isolate(test  <- updateData$datos.prueba)
      isolate(var   <- updateData$variable.predecir)
      tryCatch({
      if(!is.null(test) & length(levels(test[,var])) == 2) {
        res    <- list(list(type = "line", color = "#5F5C5C" , data = list(c(1,0), c(0,1)))) # Punto inicial del gráfico
        n2     <- 0
        i      <- 2
        
        # Recorre y obtiene la cantidad de modelos
        for (nom in names(mdls)) { 
          if(!is.null(mdls[[nom]])){
            for (alg in mdls[[nom]]) {
              n2 <- n2 + 1
            } 
          }
        }   
        # Genera colores de las líneas para cada modelo
        colores    <- gg_color_hue(n2)
        for (nom in names(mdls)) {
          if(!is.null(mdls[[nom]])){
            for (alg in mdls[[nom]]) {
              if(!startsWith(alg$nombre, "rlr")){
                roc.data <- roc.values( alg$prob$prediction[,category], test[,var]) # Calcula los puntos para el gráfico
              }else{
                roc.data <- roc.values(alg$prob$prediction[,category,], test[,var])# Calcula los puntos para el gráfico en RLR
              }
              # Se agregan al gráfico para crear la línea de la curva del modelo
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
              i <- i + 1 # Aumenta el contador para los índices
            } 
          }
        }
        # Configuraciones
        opts <- list(
          xAxis = list(show = TRUE, inverse = TRUE),
          yAxis = list(show = TRUE),
          series = res)
        
        # Crea el gráfico
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
 
