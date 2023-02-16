#' qda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qda_ui <- function(id){
  ns <- NS(id)
  
  opciones <-   
    div(
      conditionalPanel(
        "input['qda_ui_1-Boxqda'] == 'tabqdaModelo' || input['qda_ui_1-Boxqda'] == 'tabqdaProb' || input['qda_ui_1-Boxqda'] == 'tabqdaProbInd'",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
            "input['qda_ui_1-Boxqda'] == 'tabqdaModelo'",
            options.run(ns("runqda")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['qda_ui_1-Boxqda'] == 'tabqdaProb'",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = 0, max = 1, step = 0.01, 
                                      width = "100%")))
            ),
            conditionalPanel(
              "input['qda_ui_1-Boxqda'] == 'tabqdaProbInd'",
              options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                      width = "100%"))))
          
        ))))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxqda"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabqdaModelo",
               withLoader(verbatimTextOutput(ns("txtqda")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("predm"), value = "tabqdaPred",
               withLoader(DT::dataTableOutput(ns("qdaPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabqdaMC",
               withLoader(plotOutput(ns('plot_qda_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtqdaMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabqdaIndex",
               fluidRow(col_6(echarts4rOutput(ns("qdaPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("qdaErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("qdaIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("qdaIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabqdaProbInd",
               withLoader(verbatimTextOutput(ns("txtqdaprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabqdaProb",
               withLoader(verbatimTextOutput(ns("txtqdaprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' qda Server Function
#'
#' @noRd 
mod_qda_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$qda = list(n = 0, mcs = vector(mode = "list", length = 10))
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
    updateTabsetPanel(session, "Boxqda",selected = "tabqdaModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de qda
  output$txtqda <- renderPrint({
    input$runqda
    tryCatch({
    default.codigo.qda()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("qda")
    modelo <- traineR::train.qda(as.formula(var), data = train)
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
      modelos$qda[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc)
      modelos2$qda$n <- modelos2$qda$n + 1
      modelos2$qda$mcs[modelos2$qda$n] <- general.indexes(mc = mc)
      if(modelos2$qda$n > 9)
        modelos2$qda$n <- 0
      })
    nombre.modelo$x <- nombre
    print(modelo)    
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$qdaPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$qda[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtqdaMC    <- renderPrint({
    print(modelos$qda[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_qda_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$qda[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$qdaIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.qda <- indices.generales(modelos$qda[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.qda,"qda", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$qdaIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.qda <- indices.generales(modelos$qda[[nombre.modelo$x]]$mc)

    #Gráfico de Error y Precisión Global
    output$qdaPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.qda[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$qdaErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.qda[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.qda,"qda"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Genera la probabilidad de corte
  output$txtqdaprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$qda[[nombre.modelo$x]]$prob 
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
  output$txtqdaprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$qda[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$qda[[nombre.modelo$x]]$mc   <- results$MC
        modelos$qda[[nombre.modelo$x]]$pred <- results$Prediccion
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
  
  #Código por defecto de qda
  default.codigo.qda <- function() {
    
    #Modelo
    codigo <- codigo.modelo("qda", updateData$variable.predecir)
    cod    <- paste0("### adc\n",codigo)

    #Predicción
    codigo <- codigo.prediccion("qda")
    cod    <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("qda")
    cod    <- paste0(cod,codigo)
    
    #Indices generales
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.qda)\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_qda_ui("qda_ui_1")
    
## To be copied in the server
# callModule(mod_qda_server, "qda_ui_1")
 
