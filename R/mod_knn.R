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
  
opc_knn <- div(conditionalPanel(
  "input['knn_ui_1-BoxKnn'] == 'tabKknModelo' || input['knn_ui_1-BoxKnn'] == 'tabKnnProb' || input['knn_ui_1-BoxKnn'] == 'tabKnnProbInd'",
  tabsOptions(heights = c(80), tabs.content = list(
    list(conditionalPanel("input['knn_ui_1-BoxKnn']   == 'tabKknModelo'",
                          options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                          fluidRow(col_6(
                            numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                            col_6(
                              selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = 1,
                                          choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                      "triweight", "cos","inv","gaussian")))),
                          fluidRow(col_6(
                            radioSwitch(ns("switch.scale.knn"), "escal", c("si", "no"))))),
         conditionalPanel(
           "input['knn_ui_1-BoxKnn'] == 'tabKnnProb'",
           options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
           div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                  choices =  "", width = "100%"))),
           div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = 0, max = 1, step = 0.01, 
                                   width = "100%")))
         ),
         conditionalPanel(
           "input['knn_ui_1-BoxKnn'] == 'tabKnnProbInd'",
           options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
           div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                  choices =  "", width = "100%"))),
           div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                   width = "100%"))))
         )))))

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
               fluidRow(col_12(shiny::tableOutput(ns("knnIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabKnnProbInd",
               withLoader(verbatimTextOutput(ns("txtknnprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabKnnProb",
               withLoader(verbatimTextOutput(ns("txtknnprob")), 
                          type = "html", loader = "loader4"))
      )
  )
}
    
#' knn Server Function
#'
#' @noRd 
mod_knn_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$knn = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    variable <- updateData$variable.predecir
    datos    <- updateData$datos
    choices  <- as.character(unique(datos[, variable]))
    if(length(choices) == 2){
      updateSelectInput(session, "cat_probC", choices = choices, selected = choices[1])
      updateSelectInput(session, "cat.sel.prob", choices = choices, selected = choices[1])
    }else{
      updateSelectInput(session, "cat.sel.prob", choices = "")
      updateSelectInput(session, "cat_probC", choices = "")
    }
    if(!is.null(updateData$datos.aprendizaje)){
      k.value <- round(sqrt(nrow(updateData$datos.aprendizaje)))
      updateNumericInput(session,"kmax.knn",value = k.value)
    }
    updateTabsetPanel(session, "BoxKnn",selected = "tabKknModelo")
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
    modelo <- traineR::train.knn(as.formula(var), data = train, scale = as.logical(scales), kernel = kernel, kmax = k.value) 
    prob   <- predict(modelo , test, type = 'prob')
    
    variable   <- updateData$variable.predecir
    choices    <- levels(test[, variable])
    
    if(length(choices) == 2){
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      Score      <- prob$prediction[,category]
      Clase      <- test[,variable]
      results    <- prob.values.ind(Score, Clase, choices, category, corte, print = FALSE)
      mc     <- results$MC
      pred   <- results$Prediccion
    }else{
      pred   <- predict(modelo , test, type = 'class')
      mc     <- confusion.matrix(test, pred)
      pred   <- pred$prediction
    }
    
    isolate({
      modelos$knn[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc)
      modelos2$knn$n <- modelos2$knn$n + 1
      modelos2$knn$mcs[modelos2$knn$n] <- general.indexes(mc = mc)
      if(modelos2$knn$n > 9)
        modelos2$knn$n <- 0
      
      })
    
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
    idioma <- codedioma$idioma
    obj.predic(modelos$knn[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  
  #Texto de la Matríz de Confusión
  output$txtknnMC    <- renderPrint({
    print(modelos$knn[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_knn_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot_MC_code(idioma = idioma))
    plot.MC(modelos$knn[[nombre.modelo$x]]$mc)
  })
  
  
  #Tabla de Indices por Categoría 
  output$knnIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.knn <- indices.generales(modelos$knn[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.knn,"KNN", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$knnIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    
    indices.knn <- indices.generales(modelos$knn[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$knnPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.knn[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$knnErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.knn[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.knn,"KNN"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Genera la probabilidad de corte
  output$txtknnprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$knn[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      prob.values(Score, Clase, choices, category, paso)  
    },error = function(e){
      if(length(choices) != 2){
        showNotification(paste0("ERROR Probabilidad de Corte: ", tr("errorprobC", codedioma$idioma)), type = "error")
      }else{
        showNotification(paste0("ERROR: ", e), type = "error")
      }
      return(invisible(""))
      
    })
  })
  
  # Genera la probabilidad de corte
  output$txtknnprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$knn[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$knn[[nombre.modelo$x]]$mc   <- results$MC
        modelos$knn[[nombre.modelo$x]]$pred <- results$Prediccion
      }

    },error = function(e){
      if(length(choices) != 2){
        showNotification(paste0("ERROR Probabilidad de Corte: ", tr("errorprobC", codedioma$idioma)), type = "error")
      }else{
        showNotification(paste0("ERROR: ", e), type = "error")
      }
      return(invisible(""))
      
    })
  })
  
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
    codigo <-  code.kkn.modelo(updateData$variable.predecir, isolate(input$switch.scale.knn), k.value, kernel = kernel)

    cod  <- paste0("### knnl\n",codigo)
    
    # Se genera el código de la prediccion
    codigo <- codigo.prediccion("knn",  kernel)
    cod    <- paste0(cod,codigo)
    
    # Se genera el código de la matriz
    codigo <- codigo.MC("knn",  kernel)
    cod    <- paste0(cod,codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo <- paste0(codigo,"\nindices.generales(MC.knn.",kernel,")\n")
    cod    <- paste0(cod,codigo)
    isolate(codedioma$code <- append(codedioma$code, cod))
    
  }
}   
  
## To be copied in the UI
# mod_knn_ui("knn_ui_1")
    
## To be copied in the server
# callModule(mod_knn_server, "knn_ui_1")
 
