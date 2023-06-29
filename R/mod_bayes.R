#' bayes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bayes_ui <- function(id){
  ns <- NS(id)
  
  opciones <-   
    div(
      conditionalPanel(
        "input['bayes_ui_1-BoxBayes'] == 'tabBayesModelo' || input['bayes_ui_1-BoxBayes'] == 'tabBayesProb' || input['bayes_ui_1-BoxBayes'] == 'tabBayesProbInd'",
        tabsOptions(heights = c(70), tabs.content = list(
          list(conditionalPanel("input['bayes_ui_1-BoxBayes']   == 'tabBayesModelo'",
            options.run(ns("runBayes")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['bayes_ui_1-BoxBayes'] == 'tabBayesProb'",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                      width = "100%")))
            ),
            conditionalPanel(
              "input['bayes_ui_1-BoxBayes'] == 'tabBayesProbInd'",
              options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                      width = "100%")))))
          
        )))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxBayes"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabBayesModelo",
               withLoader(verbatimTextOutput(ns("txtbayes")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabBayesPred",
               withLoader(DT::dataTableOutput(ns("bayesPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabBayesMC",
               withLoader(plotOutput(ns('plot_bayes_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtbayesMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabBayesIndex",
               fluidRow(col_6(echarts4rOutput(ns("bayesPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("bayesErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("bayesIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("bayesIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabBayesProbInd",
               withLoader(verbatimTextOutput(ns("txtbayesprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabBayesProb",
               withLoader(verbatimTextOutput(ns("txtbayesprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' bayes Server Function
#'
#' @noRd 
mod_bayes_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$bayes = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    variable <- updateData$variable.predecir
    datos    <- updateData$datos
    choices  <- as.character(unique(datos[, variable]))
    if(length(choices) == 2){ # Verifica que la variable a predecir tenga solo 2 categorías
      updateSelectInput(session, "cat_probC", choices = choices, selected = choices[1]) # Actualiza las categorías para probabilidad de corte Individual
      updateSelectInput(session, "cat.sel.prob", choices = choices, selected = choices[1])# Actualiza las categorías para probabilidad de corte múltiple
    }else{
      updateSelectInput(session, "cat.sel.prob", choices = "")
      updateSelectInput(session, "cat_probC", choices = "")
    }
    updateTabsetPanel(session, "BoxBayes",selected = "tabBayesModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de bayes
  output$txtbayes <- renderPrint({
    input$runBayes
    tryCatch({
    default.codigo.bayes()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("Bayes")
    modelo <- traineR::train.bayes(as.formula(var), data = train)
    prob   <- predict(modelo , test, type = 'prob')
    
    variable   <- updateData$variable.predecir
    choices    <- levels(test[, variable])
    
    if(length(choices) == 2){
      # Para variable a predecir con 2 categorías
      category   <- isolate(input$cat_probC) # Obtiene la categoría seleccionada
      corte      <- isolate(input$val_probC) # Obtiene la probabilidad de corte seleccionada
      Score      <- prob$prediction[,category] # Guarda Scores
      Clase      <- test[,variable] # Guarda clases de la variable a predecir
      results    <- prob.values.ind(Score, Clase, choices, category, corte, print = FALSE) # Calcula las predicciones y MC
      mc     <- results$MC
      pred   <- results$Prediccion
    }else{
      # Para variable a predecir con más de 2 categorías
      pred   <- predict(modelo , test, type = 'class')
      mc     <- confusion.matrix(test, pred)
      pred   <- pred$prediction
    }
    
    isolate({
      modelos$bayes[[nombre]] <- list(nombre = nombre, modelo = modelo, pred = pred, prob = prob, mc = mc)
      
      modelos2$bayes$n <- modelos2$bayes$n + 1 # Aumenta contador de modelos generados
      modelos2$bayes$mcs[modelos2$bayes$n] <- general.indexes(mc = mc) # Guarda MC 
      if(modelos2$bayes$n > 9) # Verifica que no hayan mas de 10 modelos en "cola"
        modelos2$bayes$n <- 0 # Reinicia el contador de modelos
      })
    nombre.modelo$x <- nombre
    print(modelo)    
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$bayesPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$bayes[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtbayesMC    <- renderPrint({
    print(modelos$bayes[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_bayes_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot_MC_code(idioma = idioma))
    plot.MC(modelos$bayes[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$bayesIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.bayes <- indices.generales(modelos$bayes[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.bayes,"bayes", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$bayesIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.bayes <- indices.generales(modelos$bayes[[nombre.modelo$x]]$mc)

    #Gráfico de Error y Precisión Global
    output$bayesPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.bayes[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$bayesErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.bayes[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.bayes,"bayes"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Genera la probabilidad de corte múltiple
  output$txtbayesprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba 
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob) # Categoría seleccionada
      paso       <- isolate(input$by.prob) # Paso seleccionado para ir calculando la prob de corte
      prediccion <- modelos$bayes[[nombre.modelo$x]]$prob  # Probabilidades del modelo
      Score      <- prediccion$prediction[,category] # Probabilidades del modelo en la categoría seleccionada
      Clase      <- test[,variable] # Valor real
      prob.values(Score, Clase, choices, category, paso)  # Se calculan las MC para la categoría y paso seleccionado
    },error = function(e){
      if(length(choices) != 2){
        showNotification(paste0("ERROR Probabilidad de Corte: ", tr("errorprobC", codedioma$idioma)), type = "error")
      }else{
        showNotification(paste0("ERROR: ", e), type = "error")
      }
      return(invisible(""))
      
    })
  })
  
  # Genera la probabilidad de corte Individual
  output$txtbayesprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable]) # Niveles de la variable a predecir
      category   <- isolate(input$cat_probC) # Categoría seleccionada
      corte      <- isolate(input$val_probC) # Probabilidad de corte seleccionado, por defecto 0.5
      prediccion <- modelos$bayes[[nombre.modelo$x]]$prob # Predicción con probabilidades 
      Score      <- prediccion$prediction[,category] # Probabilidades de la categoría seleccionada
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte) # Calcula predicción y MC
        modelos$bayes[[nombre.modelo$x]]$mc   <- results$MC
        modelos$bayes[[nombre.modelo$x]]$pred <- results$Prediccion
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
  
  #Código por defecto de bayes
  default.codigo.bayes <- function() {
    
    #Modelo
    codigo <- codigo.modelo("bayes", updateData$variable.predecir)
    cod    <- paste0("### Bayes\n",codigo)

    #Predicción
    codigo <- codigo.prediccion("bayes")
    cod  <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("bayes")
    cod  <- paste0(cod,codigo)
    
    #Indices generales
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.bayes)\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_bayes_ui("bayes_ui_1")
    
## To be copied in the server
# callModule(mod_bayes_server, "bayes_ui_1")
 
