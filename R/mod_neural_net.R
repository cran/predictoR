#' neural_net UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_neural_net_ui <- function(id){
  ns <- NS(id)
  opciones.nn <- list(
        conditionalPanel("input['neural_net_ui_1-BoxNn']   == 'tabNnModelo'",
                     options.run(ns("runNn")), tags$hr(style = "margin-top: 0px;"),

                      fluidRow(col_6(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                                   min = 0, step = 0.01, value = 0.05)),
                               col_6(numericInput(ns("stepmax.nn"),labelInput("stepmax"),
                                                   min = 100, step = 100, value = 5000))),
                      fluidRow(col_12(sliderInput(inputId = ns("cant.capas.nn"), min = 1, max = 10,
                                                  label = labelInput("selectCapas"), value = 2))),
                      fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(col_2(numericInput(ns(paste0("nn.cap.",i)), NULL,
                                                                                                min = 1, step = 1, value = 2),
                                                                                   class = "mini-numeric-select"))))))
  
  codigo.nn <- list(conditionalPanel("input['neural_net_ui_1-BoxNn']  == 'tabNnPlot'",
                                     codigo.monokai(ns("fieldCodeNnPlot"), height = "10vh")),
                    conditionalPanel("input['neural_net_ui_1-BoxNn']  == 'tabNnPred'",
                                     codigo.monokai(ns("fieldCodeNnPred"), height = "10vh")),
                    conditionalPanel("input['neural_net_ui_1-BoxNn']  == 'tabNnMC'",
                                     codigo.monokai(ns("fieldCodeNnMC"), height = "10vh")),
                    conditionalPanel("input['neural_net_ui_1-BoxNn']  == 'tabNnIndex'",
                                     codigo.monokai(ns("fieldCodeNnIG"), height = "10vh")))
  
  codigo.nn.run <- list(conditionalPanel("input['neural_net_ui_1-BoxNn']  == 'tabNnModelo'",
                                     codigo.monokai(ns("fieldCodeNn"), height = "10vh")))
  opc_nn <- div(
    conditionalPanel(
    "input['neural_net_ui_1-BoxNn']   == 'tabNnModelo'",
    tabsOptions(heights = c(70, 30), tabs.content = list(
      list(options.run(ns("runNn")), tags$hr(style = "margin-top: 0px;"),
           
           fluidRow(col_6(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                       min = 0, step = 0.01, value = 0.05)),
                    col_6(numericInput(ns("stepmax.nn"),labelInput("stepmax"),
                                       min = 100, step = 100, value = 5000))),
           fluidRow(col_12(sliderInput(inputId = ns("cant.capas.nn"), min = 1, max = 10,
                                       label = labelInput("selectCapas"), value = 2))),
           fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(col_2(numericInput(ns(paste0("nn.cap.",i)), NULL,
                                                                                               min = 1, step = 1, value = 2),
                                                                                  class = "mini-numeric-select"))))),
      codigo.nn.run))),
    conditionalPanel(
      "input['neural_net_ui_1-BoxNn']   != 'tabNnModelo'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        codigo.nn))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxNn"), opciones = opc_nn,
      tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
               withLoader(verbatimTextOutput(ns("txtnn")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
               withLoader(plotOutput(ns('plot_nn'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabNnPred",
               withLoader(DT::dataTableOutput(ns("nnPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabNnMC",
               withLoader(plotOutput(ns('plot_nn_mc'), height = "45vh"), 
               type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtnnMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabNnIndex",
               fluidRow(col_6(echarts4rOutput(ns("nnPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("nnErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("nnIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("nnIndErrTable")))))
    )
  )
}
    
#' neural_net Server Function
#'
#' @noRd 
mod_neural_net_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  # Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxNn",selected = "tabNnModelo")
    default.codigo.nn()
  })

  
  # Actualiza la cantidad de capas ocultas
  observeEvent(c(input$cant.capas.nn, updateData$datos.aprendizaje), {
    if(!is.null(updateData$datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  })
  
  # Genera el texto del modelo, predicción y mc de redes
  output$txtnn <- renderPrint({
    input$runNn
    default.codigo.nn()
    train      <- updateData$datos.aprendizaje
    test       <- updateData$datos.prueba
    form       <- paste0(updateData$variable.predecir, "~.")
    nombre     <- paste0("nn")
    idioma     <- updateData$idioma
    threshold  <- isolate(input$threshold.nn)
    stepmax    <- isolate(input$stepmax.nn)
    tryCatch({
      capas      <- c(isolate(input$nn.cap.1),isolate(input$nn.cap.2),
                      isolate(input$nn.cap.3),isolate(input$nn.cap.4),
                      isolate(input$nn.cap.5),isolate(input$nn.cap.6),
                      isolate(input$nn.cap.7),isolate(input$nn.cap.8),
                      isolate(input$nn.cap.9),isolate(input$nn.cap.10))
      cant.capas <- isolate(input$cant.capas.nn)
      capas      <<- as.vector(as.numeric(capas[1:cant.capas]))
      
      modelo     <- traineR::train.neuralnet(
        formula   = as.formula(form),
        data      = train,
        threshold = threshold,
        stepmax   = stepmax,
        hidden    = capas)
      
      pred   <- predict(modelo , test, type = 'class')
      prob   <- predict(modelo , test, type = 'prob')
      mc     <- confusion.matrix(test, pred)
      isolate(modelos$nn[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
      nombre.modelo$x <- nombre
      print(modelo)
      
    },
    error = function(e) {
      showNotification(paste0("Error (NN-10) : ",e), duration = 15, type = "error")
      return(invisible(""))
    },
    warning = function(w){
      showNotification(paste0(tr("nnWar", idioma)," (NN) : ",w), duration = 10, type = "warning")
      return(invisible(""))
    })


  })
  
  #Tabla de la predicción
  output$nnPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- updateData$idioma
    obj.predic(modelos$nn[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtnnMC    <- renderPrint({
    print(modelos$nn[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_nn_mc <- renderPlot({
    idioma <- updateData$idioma
    tryCatch({  
      exe(plot.MC.code(idioma = idioma))
      plot.MC(modelos$nn[[nombre.modelo$x]]$mc)
    },
    error = function(e) { 
      showNotification(paste0("Error (NN) : ", e), duration = 15, type = "error")
      return(NULL)
    })

  })
  
  #Tabla de Indices por Categoría 
  output$nnIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.nn <- indices.generales(modelos$nn[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.nn,"nn", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$nnIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.nn <- indices.generales(modelos$nn[[nombre.modelo$x]]$mc)
    # Overall accuracy and overall error plot
    output$nnPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.nn[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$nnErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.nn[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.nn,"nn"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Actualiza el código a la versión por defecto
  default.codigo.nn <- function(){
    #Modelo
    codigo <- nn.modelo(updateData$variable.predecir,
                        isolate(input$threshold.nn),
                        isolate(input$stepmax.nn),
                        isolate(input$cant.capas.nn),
                        isolate(input$nn.cap.1),isolate(input$nn.cap.2),
                        isolate(input$nn.cap.3),isolate(input$nn.cap.4),
                        isolate(input$nn.cap.5),isolate(input$nn.cap.6),
                        isolate(input$nn.cap.7),isolate(input$nn.cap.8),
                        isolate(input$nn.cap.9),isolate(input$nn.cap.10))

    updateAceEditor(session, "fieldCodeNn", value = codigo)

    #Neuralnet PLot
    updateAceEditor(session, "fieldCodeNnPlot", value = nn.plot())

    #Predicción
    codigo <- nn.prediccion()
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)

    #Matriz de Confusión
    codigo <- nn.MC()
    updateAceEditor(session, "fieldCodeNnMC", value = codigo)

    #Indices Generales
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
  }
  
  #Genera el gráfico de la red neuronal
  output$plot_nn <- renderPlot({
    idioma <- updateData$idioma
    tryCatch({
      capas <- c(input$nn.cap.1, input$nn.cap.2,
                 input$nn.cap.3, input$nn.cap.4,
                 input$nn.cap.5, input$nn.cap.6,
                 input$nn.cap.7, input$nn.cap.8,
                 input$nn.cap.9, input$nn.cap.10)
      cant  <- isolate(input$cant.capas.nn)
      capas <- capas[1:cant]
      modelo <- modelos$nn[[nombre.modelo$x]]$modelo
      if(cant * sum(capas) <= 1500 & ncol(modelo$covariate) <= 20){

        cod <- ifelse(input$fieldCodeNnPlot == "", nn.plot(), input$fieldCodeNnPlot)
        plot(modelo,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,
        information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',
         dimension=15, radius = 0.2, fontsize = 10)
      }else{
        showNotification(tr("bigPlot",idioma), duration = 10, type = "message")
        return(NULL)
      }
    },
    error = function(e){
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_neural_net_ui("neural_net_ui_1")
    
## To be copied in the server
# callModule(mod_neural_net_server, "neural_net_ui_1", updateData)
 
