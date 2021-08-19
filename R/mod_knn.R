#' knn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_knn_ui <- function(id){
  ns <- NS(id)
  opciones.knn <- list(options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                       conditionalPanel("input['knn_ui_1-BoxKnn'] == 'tabKknModelo'",
                           fluidRow(col_6(
                                          numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                                    col_6(
                                          selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = 1,
                                                       choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                   "triweight", "cos","inv","gaussian")))),
                           fluidRow(col_6(
                                          radioSwitch(ns("switch.scale.knn"), "escal", c("si", "no"))))))
  
  codigo.knn <- list(conditionalPanel("input['knn_ui_1-BoxKnn'] == 'tabKknPred'",
                                      codigo.monokai(ns("fieldCodeKnnPred"),height = "10vh")),
                     conditionalPanel("input['knn_ui_1-BoxKnn'] == 'tabKknMC'",
                                      codigo.monokai(ns("fieldCodeKnnMC"),height = "10vh")),
                     conditionalPanel("input['knn_ui_1-BoxKnn'] == 'tabKknIndex'",
                                      codigo.monokai(ns("fieldCodeKnnIG"),height = "10vh")))  
  codigo.knn.run <- list(conditionalPanel("input['knn_ui_1-BoxKnn'] == 'tabKknModelo'",
                                      codigo.monokai(ns("fieldCodeKnn"), height = "10vh")))
  
opc_knn <- div(conditionalPanel(
  "input['knn_ui_1-BoxKnn'] == 'tabKknModelo'",
  tabsOptions(heights = c(70, 30), tabs.content = list(
    list(options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                          fluidRow(col_6(
                            numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                            col_6(
                              selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = 1,
                                          choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                      "triweight", "cos","inv","gaussian")))),
                          fluidRow(col_6(
                            radioSwitch(ns("switch.scale.knn"), "escal", c("si", "no"))))),
          codigo.knn.run))),
  conditionalPanel(
    "input['knn_ui_1-BoxKnn'] != 'tabKknModelo'",
    tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
      codigo.knn))))

  tagList(
    tabBoxPrmdt(
      id = ns("BoxKnn"), opciones = opc_knn,
      tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
               withLoader(verbatimTextOutput(ns("txtknn")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabKknPred",
               withLoader(DT::dataTableOutput(ns("knnPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabKknMC",
               withLoader(plotOutput(ns('plot_knn_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtknnMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabKknIndex",
               fluidRow(col_6(echarts4rOutput(ns("knnPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("knnErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("knnIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("knnIndErrTable")))))
      )
  )
}
    
#' knn Server Function
#'
#' @noRd 
mod_knn_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  #requireNamespace("traineR")
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    updateTabsetPanel(session, "BoxKnn",selected = "tabKknModelo")
    default.codigo.knn(k.def = TRUE)
  })

  # Genera el texto del modelo, predicción y mc de knn
  output$txtknn <- renderPrint({
    input$runKnn
    tryCatch({
    default.codigo.knn()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    scales <- isolate(input$switch.scale.knn)
    kernel <- isolate(input$kernel.knn)
    k.value<- isolate(input$kmax.knn)
    nombre <- paste0("knnl-",kernel)
    modelo <- traineR::train.knn(as.formula(var), data = train, scale = as.logical(scales), kernel = kernel, kmax = k.value ) 
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$knn[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
    nombre.modelo$x <- nombre
    print(modelo)
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$knnPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- updateData$idioma
    obj.predic(modelos$knn[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  
  #Texto de la Matríz de Confusión
  output$txtknnMC    <- renderPrint({
    print(modelos$knn[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_knn_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$knn[[nombre.modelo$x]]$mc)
  })
  
  
  #Tabla de Indices por Categoría 
  output$knnIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.knn <- indices.generales(modelos$knn[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.knn,"KNN", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$knnIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.knn <- indices.generales(modelos$knn[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$knnPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.knn[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$knnErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.knn[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.knn,"KNN"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Actualiza el código a la versión por defecto
  default.codigo.knn <- function(k.def = FALSE) {
    train  <- updateData$datos.aprendizaje
    if(!is.null(train) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(train))), isolate(input$kmax.knn))
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- isolate(input$kmax.knn)
    }
    
    kernel <-  isolate(input$kernel.knn)
    codigo <- code.kkn.modelo(updateData$variable.predecir, isolate(input$switch.scale.knn), k.value, kernel = kernel)
    updateAceEditor(session, "fieldCodeKnn", value = codigo)

    # Se genera el código de la prediccion
    codigo       <- kkn.prediccion(kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)

    # Se genera el código de la matriz
    codigo       <- knn.MC(kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnMC", value = codigo)

    # Se genera el código de la indices
    codigo       <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
  }
}   
  
## To be copied in the UI
# mod_knn_ui("knn_ui_1")
    
## To be copied in the server
# callModule(mod_knn_server, "knn_ui_1")
 
