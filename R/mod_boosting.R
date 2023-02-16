#' boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boosting_ui <- function(id){
  ns <- NS(id)

  opciones <-   
    div(
      conditionalPanel(
        "input['boosting_ui_1-BoxB'] == 'tabBModelo' || input['boosting_ui_1-BoxB'] == 'tabBRules' || input['boosting_ui_1-BoxB'] == 'tabBProb' || input['boosting_ui_1-BoxB'] == 'tabBProbInd'",
        tabsOptions(heights = c(70), tabs.content = list(
          list(
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBModelo'",
              options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
              div(col_6(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1)),
                       col_6(numericInput(ns("maxdepth.boosting"), labelInput("maxdepth"), 15, width = "100%",min = 1))),
              
              div(col_6(numericInput(ns("minsplit.boosting"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                  col_6(
                    selectInput(inputId = ns("coeff.boosting"), label = labelInput("selkernel"),selected = 1,
                                choices = c("Breiman", "Freund", "Zhu"))))),
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBRules'",
              options.base(), tags$hr(style = "margin-top: 0px;"),
              numericInput(ns("rules.b.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1)),
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBProb'",
              options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                      width = "100%")))
            ),
            conditionalPanel(
              "input['boosting_ui_1-BoxB'] == 'tabBProbInd'",
              options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
              div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                     choices =  "", width = "100%"))),
              div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                      width = "100%")))))
          
        )))
    )

  tagList(
    tabBoxPrmdt(
      id = ns("BoxB"), opciones = opciones,
      tabPanel(title = labelInput("generatem"), value = "tabBModelo",
               withLoader(verbatimTextOutput(ns("txtBoosting")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("evolerror"), value = "tabBError",
               withLoader(echarts4rOutput(ns('plot_boosting_error'), height = "55vh"), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabBImp",
               withLoader(echarts4rOutput(ns('plot_boosting_import'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabBPred",
               withLoader(DT::dataTableOutput(ns("boostingPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabBMC",
               withLoader(plotOutput(ns('plot_boosting_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtBoostingMC"))),
      
      tabPanel(title = labelInput("indices"),value = "tabBIndex",
               div(col_6(echarts4rOutput(ns("boostingPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("boostingErrorGlob"), width = "100%"))),
               div(col_12(shiny::tableOutput(ns("boostingIndPrecTable")))),
               div(col_12(shiny::tableOutput(ns("boostingIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"), value = "tabBRules",
               verbatimTextOutput(ns("rulesB"))),
      tabPanel(title = labelInput("probC"), value = "tabBProbInd",
               withLoader(verbatimTextOutput(ns("txtboostprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabBProb",
               withLoader(verbatimTextOutput(ns("txtboostprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}
    
#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$boosting = list(n = 0, mcs = vector(mode = "list", length = 10))
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
    updateTabsetPanel(session, "BoxB",selected = "tabBModelo")
    
    
  })
  
  # Genera el texto del modelo, predicción y mc de boosting
  output$txtBoosting <- renderPrint({
    input$runBoosting
    tryCatch({
    default.codigo.boosting()
    train   <- updateData$datos.aprendizaje
    test    <- updateData$datos.prueba
    var     <- paste0(updateData$variable.predecir, "~.")
    iter    <- isolate(input$iter.boosting)
    maxdepth<-isolate(input$maxdepth.boosting)
    minsplit<-isolate(input$minsplit.boosting) 
    coeff   <-isolate(input$coeff.boosting)
    nombre  <- paste0("bl")
    modelo  <- traineR::train.adabag(as.formula(var), data = train, mfinal = iter,coeflearn = coeff,
                                    control = rpart.control(minsplit =minsplit, maxdepth = maxdepth))
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
      modelos$boosting[[nombre]] <- list(nombre = nombre, modelo = modelo, pred = pred, prob = prob , mc = mc)
      modelos2$boosting$n <- modelos2$boosting$n + 1
      modelos2$boosting$mcs[modelos2$boosting$n] <- general.indexes(mc = mc)
      if(modelos2$boosting$n > 9)
        modelos2$boosting$n <- 0
    
      })
    nombre.modelo$x <- nombre
    print(modelo)
    }, error = function(e) {
      return(invisible(""))
    })
  })

  #Tabla de la predicción
  output$boostingPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$boosting[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtBoostingMC    <- renderPrint({
    print(modelos$boosting[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_boosting_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$boosting[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$boostingIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.boosting <- indices.generales(modelos$boosting[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.boosting,"boosting", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$boostingIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.boosting <- indices.generales(modelos$boosting[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$boostingPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.boosting[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$boostingErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.boosting[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.boosting,"boosting"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Mostrar Reglas
  output$rulesB <- renderPrint({
    n <- input$rules.b.n
    isolate(train   <- updateData$datos.aprendizaje)
    isolate(var.pred<- updateData$variable.predecir)
    tryCatch({
      isolate(codedioma$code <- append(codedioma$code, rules.boosting(n)))
      rules(modelos$boosting[[nombre.modelo$x]]$modelo$trees[[n]], train, var.pred)
    },error = function(e) {
      stop(tr("NoDRule", codedioma$idioma))
    }
  )})
  
  # Genera la probabilidad de corte
  output$txtboostprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$boosting[[nombre.modelo$x]]$prob 
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
  output$txtboostprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$boosting[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$boosting[[nombre.modelo$x]]$mc   <- results$MC
        modelos$boosting[[nombre.modelo$x]]$pred <- results$Prediccion
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
  
  # Gráfico de importancia boosting
  output$plot_boosting_import <- renderEcharts4r({
    cod <- ifelse(input$fieldCodeBoostingPlotImport == "",boosting.plot.import(),input$fieldCodeBoostingPlotImport)
    tryCatch({
      imp   <- modelos$boosting[[nombre.modelo$x]]$modelo$importance
      color <- gg_color_hue(length(imp))
      aux   <- data.frame(importancia = imp, color = color) 
      aux$nombre      <- row.names(aux) 
      aux$importancia <- abs(aux$importancia) 
      aux <- aux[order(aux$importancia, decreasing = T), ]
      aux |>  e_charts(nombre) |>  e_bar(importancia, name = var) |>   
        e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|>
        e_add_nested("itemStyle", color) |>   
        e_flip_coords() |>  
        e_y_axis(inverse = TRUE)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Gráfico de evolución del error boosting
  output$plot_boosting_error <- renderEcharts4r({
    train  <- updateData$datos.aprendizaje
    modelo <- modelos$boosting[[nombre.modelo$x]]$modelo
    cod <- ifelse(input$fieldCodeBoostingPlot == "",boosting.plot(),input$fieldCodeBoostingPlot)
    tryCatch({
      error(modelo, train) -> evol.train
      e_evol_error(evol.train,strsplit(tr("numTree", codedioma$idioma), ':')[[1]])
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Actualiza el código a la versión por defecto
  default.codigo.boosting <- function() {
    
    # Se actualiza el código del modelo
    codigo <- boosting.modelo(variable.pr = updateData$variable.predecir,
                              iter        = isolate(input$iter.boosting),
                              maxdepth    = isolate(input$maxdepth.boosting),
                              minsplit    = isolate(input$minsplit.boosting))
    
    cod  <- paste0("### docpot\n",codigo)
    
    #Predicción
    codigo <- codigo.prediccion("boosting")
    cod  <- paste0(cod,codigo)
    
    #Matríz de Confusión
    codigo <- codigo.MC("boosting")
    cod  <- paste0(cod,codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo <- paste0(codigo,"\nindices.generales(MC.boosting)\n")
    
    
    cod  <- paste0(cod, codigo)
    # Cambia el código del gráfico del modelo
    cod  <- paste0(cod, "### evolerror\n",boosting.plot())
    # Cambia el código del gráfico de importancia
    cod  <- paste0(cod, "### docImpV\n",boosting.plot.import())
    
    isolate(codedioma$code <- append(codedioma$code, cod))
    
  }
  
}
    
## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")
    
## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")
 
