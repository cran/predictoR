#' r_forest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom shiny NS tagList 
mod_r_forest_ui <- function(id){
  ns <- NS(id)
  
  codigo.rf  <- list(conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRferror'",
                                      codigo.monokai(ns("fieldCodeRfPlotError"), height = "10vh")),
                     conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfImp'",
                                      codigo.monokai(ns("fieldCodeRfPlot"), height = "10vh")),
                     conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfPred'",
                                      codigo.monokai(ns("fieldCodeRfPred"), height = "10vh")),
                     conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfMC'",
                                      codigo.monokai(ns("fieldCodeRfMC"), height = "10vh")),
                     conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfIndex'",
                                      codigo.monokai(ns("fieldCodeRfIG"), height = "10vh")))
  
  codigo.rf.run  <- list(conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfModelo'",
                                      codigo.monokai(ns("fieldCodeRf"), height = "10vh")),
                     conditionalPanel("input['r_forest_ui_1-BoxRf'] == 'tabRfRules'",
                                      codigo.monokai(ns("fieldCodeRfRules"), height = "10vh")))
  
  opc_rf  <-     div(
    conditionalPanel(
      "input['r_forest_ui_1-BoxRf'] == 'tabRfModelo' || input['r_forest_ui_1-BoxRf'] == 'tabRfRules'",
      tabsOptions(heights = c(70, 30), tabs.content = list(
        list(
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfModelo'",
            options.run(ns("runRf")), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_6(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0)),
                     col_6(numericInput(ns("mtry.rf"),labelInput("numVars"),1, width = "100%", min = 1)))),
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfRules'",
            options.base(), tags$hr(style = "margin-top: 0px;"),
            numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1))),
        codigo.rf.run
      ))),
    conditionalPanel(
      "input['r_forest_ui_1-BoxRf'] != 'tabRfModelo' && input['r_forest_ui_1-BoxRf'] != 'tabRfRules'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        codigo.rf
      )))
  )
  

  tagList(
    tabBoxPrmdt(
      id = ns("BoxRf"),opciones = opc_rf,
      tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
               withLoader(verbatimTextOutput(ns("txtRf")),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("evolerror"), value = "tabRferror",
               withLoader(echarts4rOutput(ns('plot_error_rf'), height = "55vh"),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabRfImp",
               withLoader(echarts4rOutput(ns('plot_rf_importance'), height = "55vh"),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRfPred",
               withLoader(DT::dataTableOutput(ns("rfPrediTable")),
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRfMC",
               withLoader(plotOutput(ns('plot_rf_mc'), height = "45vh"),
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtRfMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRfIndex",
               fluidRow(col_6(echarts4rOutput(ns("rfPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("rfErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rfIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rfIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"), value = "tabRfRules",
               withLoader(verbatimTextOutput(ns("rulesRf")),
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' r_forest Server Function
#'
#' @noRd 
mod_r_forest_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    default.codigo.rf(rf.def = TRUE)
    updateTabsetPanel(session, "BoxRf",selected = "tabRfModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de RF
  output$txtRf <- renderPrint({
    input$runRf
    tryCatch({
    default.codigo.rf()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    mtry   <- isolate(input$mtry.rf)
    ntree  <- isolate(input$ntree.rf)
    nombre <- paste0("rfl")
    
    modelo <- traineR::train.randomForest(as.formula(var), data = train, mtry = mtry, ntree = ntree, importance = TRUE)
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$rf[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$rfPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- updateData$idioma
    obj.predic(modelos$rf[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtRfMC    <- renderPrint({
    print(modelos$rf[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_rf_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$rf[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rfIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rf <- indices.generales(modelos$rf[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rf,"rf", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rfIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rf <- indices.generales(modelos$rf[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rfPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.rf[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$rfErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.rf[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.rf,"rf"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Mostrar Reglas
  output$rulesRf <- renderPrint({
    idioma <- updateData$idioma
    n      <- input$rules.rf.n
    modelo <- modelos$rf[[nombre.modelo$x]]$modelo
    modelo$call$data <- updateData$datos.aprendizaje
    tryCatch({
      updateAceEditor(session,"fieldCodeRfRules",paste0("rulesRandomForest(modelo.rf, ",n,")"))
      rulesRandomForest(modelo, n)
    },error = function(e){
             stop(tr("NoDRule", idioma))
    })
  })
  
  
  # Gráfico de importancia
  output$plot_rf_importance <- renderEcharts4r({
    tryCatch({
      updateAceEditor(session, "fieldCodeRfPlot", value = rf.importance.plot())
      aux <- data.frame(modelos$rf[[nombre.modelo$x]]$modelo$importance)
      aux$MeanDecreaseAccuracy <- abs(aux$MeanDecreaseAccuracy)
      aux   <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]
      label <- row.names(aux)
      color <- gg_color_hue(length(label))
      aux   <- cbind(aux,label = label, color = color)
      aux |>  e_charts(label) |>  e_bar(MeanDecreaseAccuracy, name = var) |>  
        e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|>
        e_add_nested("itemStyle", color) |>  
        e_flip_coords() |> 
        e_y_axis(inverse = TRUE) 
    }, error = function(e) {
      return(NULL)
    })
    
  })
  
  # Gráfico de evolución del error
  output$plot_error_rf <- renderEcharts4r({
    tryCatch({
      modelo    <- modelos$rf[[nombre.modelo$x]]$modelo
      updateAceEditor(session, "fieldCodeRfPlotError", value = plot.rf.error())
      e_rf_error(modelo)
    }, error = function(e){
      return(NULL)
    })
  })

  # Actualiza el código a la versión por defecto
  default.codigo.rf <- function(rf.def = FALSE){
    train  <- updateData$datos.aprendizaje
    mtry <- isolate(input$mtry.rf)
    if((!is.null(train) & rf.def) | is.na(mtry)){
      mtry.value <- ifelse(rf.def || is.na(mtry), round(sqrt(ncol(train))), mtry)
      if(!is.na(mtry)){
        updateNumericInput(session,"mtry.rf",value = mtry.value)
      }
    }else{
      mtry.value <- mtry
    }
    
    # Se actualiza el código del modelo
    codigo <- rf.modelo(variable.pr = updateData$variable.predecir,
                        ntree = isolate(input$ntree.rf),
                        mtry = mtry.value)
    
    updateAceEditor(session, "fieldCodeRf", value = codigo)

    # Cambia el código del gráfico de rf
    updateAceEditor(session, "fieldCodeRfPlotError", value = plot.rf.error())
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.importance.plot())
     

    # Se genera el código de la predicción
    codigo <- rf.prediccion()
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)


    # Se genera el código de la matriz
    codigo <- rf.MC()
    updateAceEditor(session, "fieldCodeRfMC", value = codigo)

    # Se genera el código de los indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
  }
}
    
## To be copied in the UI
# mod_r_forest_ui("r_forest_ui_1")
    
## To be copied in the server
# callModule(mod_r_forest_server, "r_forest_ui_1")
 
