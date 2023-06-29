#' lda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_lda_ui <- function(id){
  ns <- NS(id)
  
  opciones <-   
    div(
      conditionalPanel(
        "input['lda_ui_1-Boxlda'] == 'tabldaModelo'  || input['lda_ui_1-Boxlda'] == 'tabldaProb' || input['lda_ui_1-Boxlda'] == 'tabldaProbInd'",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
            "input['lda_ui_1-Boxlda'] == 'tabldaModelo'",
            options.run(ns("runlda")), tags$hr(style = "margin-top: 0px;")),
            conditionalPanel(
              "input['lda_ui_1-Boxlda'] == 'tabldaProb'",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = 0, max = 1, step = 0.01, 
                                      width = "100%")))
            ),
            conditionalPanel(
              "input['lda_ui_1-Boxlda'] == 'tabldaProbInd'",
              options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                      width = "100%"))))
          
        ))))
    )
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxlda"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabldaModelo",
               withLoader(verbatimTextOutput(ns("txtlda")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gclasificacion"), value = "tabldaPlot",
               withLoader(plotOutput(ns('plot_lda'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabldaPred",
               withLoader(DT::dataTableOutput(ns("ldaPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabldaMC",
               withLoader(plotOutput(ns('plot_lda_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtldaMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabldaIndex",
               fluidRow(col_6(echarts4rOutput(ns("ldaPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("ldaErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("ldaIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("ldaIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabldaProbInd",
               withLoader(verbatimTextOutput(ns("txtldaprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabldaProb",
               withLoader(verbatimTextOutput(ns("txtldaprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' lda Server Function
#'
#' @noRd 
mod_lda_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$lda = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    nombres <- colnames.empty(var.numericas(updateData$datos))
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
    updateTabsetPanel(session, "Boxlda",selected = "tabldaModelo")
  })
  
  # Genera el texto del modelo, predicción y mc de lda
  output$txtlda <- renderPrint({
    input$runlda
    tryCatch({
    default.codigo.lda()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    nombre <- paste0("lda")
    modelo <- traineR::train.lda(as.formula(var), data = train)
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
      modelos$lda[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc)
      modelos2$lda$n <- modelos2$lda$n + 1
      modelos2$lda$mcs[modelos2$lda$n] <- general.indexes(mc = mc)
      if(modelos2$lda$n > 9)
        modelos2$lda$n <- 0
      })
    nombre.modelo$x <- nombre
    print(modelo)    
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$ldaPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$lda[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtldaMC    <- renderPrint({
    print(modelos$lda[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_lda_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot_MC_code(idioma = idioma))
    plot.MC(modelos$lda[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$ldaIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.lda <- indices.generales(modelos$lda[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.lda,"lda", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$ldaIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.lda <- indices.generales(modelos$lda[[nombre.modelo$x]]$mc)

    #Gráfico de Error y Precisión Global
    output$ldaPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.lda[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$ldaErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.lda[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.lda,"lda"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update LDA plot
  output$plot_lda <- renderPlot({
    tryCatch({
      idioma    <- codedioma$idioma
      train     <- updateData$datos.aprendizaje
      variable  <- isolate(updateData$variable.predecir)
      # cod       <- paste0("### gclasificacion\n",cod)
      modelo    <- modelos$lda[[nombre.modelo$x]]$modelo
        #isolate(codedioma$code <- append(codedioma$code, cod))
      plot(modelo, col = as.numeric(train[, variable]))
    },error = function(e){
      showNotification(e,
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })
  
  
  # Genera la probabilidad de corte
  output$txtldaprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$lda[[nombre.modelo$x]]$prob 
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
  output$txtldaprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$lda[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$lda[[nombre.modelo$x]]$mc   <- results$MC
        modelos$lda[[nombre.modelo$x]]$pred <- results$Prediccion
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
  
  #Código por defecto de lda
  default.codigo.lda <- function() {
    
    #Modelo
    codigo <- codigo.modelo("lda", updateData$variable.predecir)
    cod    <- paste0("### adl\n",codigo)

    #Predicción
    codigo <- codigo.prediccion("lda")
    cod  <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("lda")
    cod  <- paste0(cod,codigo)
    
    #Indices generales
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.lda)\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}
    
## To be copied in the UI
# mod_lda_ui("lda_ui_1")
    
## To be copied in the server
# callModule(mod_lda_server, "lda_ui_1")
 
