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

  opciones <-   
    div(
      conditionalPanel(
        "input['l_regression_ui_1-BoxRl'] == 'tabRlModelo' || input['l_regression_ui_1-BoxRl'] == 'tabRlProb' || input['l_regression_ui_1-BoxRl'] == 'tabRlProbInd'",
        tabsOptions(heights = c(70, 30), tabs.content = list(
          list(conditionalPanel("input['l_regression_ui_1-BoxRl']   == 'tabRlModelo'",
            options.run(ns("runRl")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['l_regression_ui_1-BoxRl'] == 'tabRlProb'",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                      width = "100%")))
            ),
            conditionalPanel(
              "input['l_regression_ui_1-BoxRl'] == 'tabRlProbInd'",
              options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                      width = "100%"))))
        ))))
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
               fluidRow(col_12(shiny::tableOutput(ns("rlIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabRlProbInd",
               withLoader(verbatimTextOutput(ns("txtrlprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabRlProb",
               withLoader(verbatimTextOutput(ns("txtrlprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' l_regression Server Function
#'
#' @noRd 
mod_l_regression_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$rl = list(n = 0, mcs = vector(mode = "list", length = 10))
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
    updateTabsetPanel(session, "BoxRl",selected = "tabRlModelo")
  })

  # Genera el texto del modelo, predicción y mc de rl
  output$txtrl <- renderPrint({
    input$runRl
    idioma <- codedioma$idioma
    
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
      modelos$rl[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob, mc = mc)
      modelos2$rl$n <- modelos2$rl$n + 1
      modelos2$rl$mcs[modelos2$rl$n] <- general.indexes(mc = mc)
      if(modelos2$rl$n > 9)
        modelos2$rl$n <- 0
      })
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
    idioma <- codedioma$idioma
    obj.predic(modelos$rl[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtrlMC    <- renderPrint({
    print(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_rl_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot_MC_code(idioma = idioma))
    plot.MC(modelos$rl[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rlIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rl,"rl", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rlIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.rl <- indices.generales(modelos$rl[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rlPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.rl[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$rlErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.rl[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.rl,"rl"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Genera la probabilidad de corte
  output$txtrlprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$rl[[nombre.modelo$x]]$prob 
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
  output$txtrlprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$rl[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$rl[[nombre.modelo$x]]$mc   <- results$MC
        modelos$rl[[nombre.modelo$x]]$pred <- results$Prediccion
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
  default.codigo.rl <- function() {
    # Se actualiza el código del modelo
    codigo <- rl.modelo(updateData$variable.predecir)
    cod  <- paste0("### rl\n",codigo)
    
    #Predicción
    codigo <- codigo.prediccion("glm")
    cod  <- paste0(cod, codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("glm")
    cod  <- paste0(cod, codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.glm)\n")
    cod  <- paste0(cod,codigo)
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_l_regression_ui("l_regression_ui_1")
    
## To be copied in the server
# callModule(mod_l_regression_server, "l_regression_ui_1", updateData)
 
