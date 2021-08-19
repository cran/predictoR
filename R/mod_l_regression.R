#' l_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_l_regression_ui <- function(id){
  ns <- NS(id)
  codigo.run <- list(conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlModelo'",
                                     codigo.monokai(ns("fieldCodeRl"), height = "10vh")))
  
  codigo.rl  <- list(conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlPred'",
                                     codigo.monokai(ns("fieldCodeRlPred"), height = "10vh")),
                     conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlMC'",
                                     codigo.monokai(ns("fieldCodeRlMC"), height = "10vh")),
                     conditionalPanel("input['l_regression_ui_1-BoxRl']  == 'tabRlIndex'",
                                     codigo.monokai(ns("fieldCodeRlIG"), height = "10vh")))
  
  opc_rl <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                         tabs.content = list(codigo.rl))
  opciones <-   
    div(
      conditionalPanel(
        "input['l_regression_ui_1-BoxRl'] == 'tabRlModelo'",
        tabsOptions(heights = c(70, 30), tabs.content = list(
          list(
            options.run(ns("runRl")), tags$hr(style = "margin-top: 0px;")),
          codigo.run
        ))),
      conditionalPanel(
        "input['l_regression_ui_1-BoxRl'] != 'tabRlModelo'",
        tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
          codigo.rl
        )))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxRl"),opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabRlModelo",
               withLoader(verbatimTextOutput(ns("txtrl")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRlPred",
               withLoader(DT::dataTableOutput(ns("rlPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRlMC",
               withLoader(plotOutput(ns('plot_rl_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtrlMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRlIndex",
               fluidRow(col_6(echarts4rOutput(ns("rlPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("rlErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rlIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rlIndErrTable")))))
    )
  )
}
    
#' l_regression Server Function
#'
#' @noRd 
mod_l_regression_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxRl",selected = "tabRlModelo")
    default.codigo.rl()
  })

  # Genera el texto del modelo, predicción y mc de rl
  output$txtrl <- renderPrint({
    input$runRl
    idioma <- updateData$idioma
    
    if (length(levels(updateData$datos[, updateData$variable.predecir])) != 2) {
      if (isFALSE(getOption("shiny.testmode")) || is.null(getOption("shiny.testmode"))) {
        showModal(modalDialog(
          title = tr("rl", idioma), tr("limitModel", idioma),
          footer = modalButton("Cerrar"), easyClose = T
        ))
        return(invisible(""))
      }
    }
    tryCatch({
    default.codigo.rl()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("rl")
    modelo <- traineR::train.glm(as.formula(var), data = train)
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$rl[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob, mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
    }, error = function(e) {
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$rlPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- updateData$idioma
    obj.predic(modelos$rl[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtrlMC    <- renderPrint({
    print(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_rl_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rlIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rl,"rl", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rlIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rlPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.rl[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$rlErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.rl[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.rl,"rl"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Actualiza el código a la versión por defecto
  default.codigo.rl <- function() {
    # Se actualiza el código del modelo
    codigo <- rl.modelo(updateData$variable.predecir)
    updateAceEditor(session, "fieldCodeRl", value = codigo)

    # Se genera el código de la prediccion
    codigo <- rl.prediccion()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- rl.MC()
    updateAceEditor(session, "fieldCodeRlMC", value = codigo)

    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
  }
}
    
## To be copied in the UI
# mod_l_regression_ui("l_regression_ui_1")
    
## To be copied in the server
# callModule(mod_l_regression_server, "l_regression_ui_1", updateData)
 
