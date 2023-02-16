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
  
  opc_rf  <-     div(
    conditionalPanel(
      "input['r_forest_ui_1-BoxRf'] == 'tabRfModelo' || input['r_forest_ui_1-BoxRf'] == 'tabRfRules' || input['r_forest_ui_1-BoxRf'] == 'tabRfProb' || input['r_forest_ui_1-BoxRf'] == 'tabRfProbInd'",
      tabsOptions(heights = c(70), tabs.content = list(
        list(
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfModelo'",
            options.run(ns("runRf")), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_6(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0)),
                     col_6(numericInput(ns("mtry.rf"),labelInput("numVars"),1, width = "100%", min = 1)))),
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfRules'",
            options.base(), tags$hr(style = "margin-top: 0px;"),
            numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1)),
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfProb'",
            options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
            div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                   choices =  "", width = "100%"))),
            div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,  
                                    width = "100%")))
          ),
          conditionalPanel(
            "input['r_forest_ui_1-BoxRf'] == 'tabRfProbInd'",
            options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
            div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                   choices =  "", width = "100%"))),
            div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                    width = "100%")))
          ))
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
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probC"), value = "tabRfProbInd",
               withLoader(verbatimTextOutput(ns("txtrfprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabRfProb",
               withLoader(verbatimTextOutput(ns("txtrfprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}

#' r_forest Server Function
#'
#' @noRd 
mod_r_forest_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$rf = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    
    variable <- updateData$variable.predecir
    datos    <- updateData$datos
    choices  <- as.character(unique(datos[, variable]))
    n.mtry   <- floor(sqrt(ncol(updateData$datos.aprendizaje)))
    if(length(choices) == 2){
      updateSelectInput(session, "cat_probC", choices = choices, selected = choices[1])
      updateSelectInput(session, "cat.sel.prob", choices = choices, selected = choices[1])
    }else{
      updateSelectInput(session, "cat.sel.prob", choices = "")
      updateSelectInput(session, "cat_probC", choices = "")
    }
    updateNumericInput(session, "mtry.rf", value = n.mtry)
    
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
        modelos$rf[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc)
        modelos2$rf$n <- modelos2$rf$n + 1
        modelos2$rf$mcs[modelos2$rf$n] <- general.indexes(mc = mc)
        if(modelos2$rf$n > 9)
          modelos2$rf$n <- 0
        })
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
    idioma <- codedioma$idioma
    obj.predic(modelos$rf[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtRfMC    <- renderPrint({
    print(modelos$rf[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_rf_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$rf[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rfIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.rf <- indices.generales(modelos$rf[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rf,"rf", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rfIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.rf <- indices.generales(modelos$rf[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rfPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.rf[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$rfErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.rf[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.rf,"rf"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Mostrar Reglas
  output$rulesRf <- renderPrint({
    idioma <- codedioma$idioma
    n      <- input$rules.rf.n
    modelo <- modelos$rf[[nombre.modelo$x]]$modelo
    modelo$call$data <- updateData$datos.aprendizaje
    tryCatch({
      isolate(codedioma$code <- append(codedioma$code, paste0("### reglas\n", "rulesRandomForest(modelo.rf, ",n,")\n")))
      
      rulesRandomForest(modelo, n)
    },error = function(e){
      stop(tr("NoDRule", idioma))
    })
  })
  
  
  # Gráfico de importancia
  output$plot_rf_importance <- renderEcharts4r({
    tryCatch({
      cod  <- paste0("### docImpV\n", rf.importance.plot())
      isolate(codedioma$code <- append(codedioma$code, cod))
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
      cod  <- paste0("### evolerror\n", plot.rf.error())
      isolate(codedioma$code <- append(codedioma$code, cod))
      e_rf_error(modelo,strsplit(tr("numTree", codedioma$idioma), ':')[[1]])
    }, error = function(e){
      return(NULL)
    })
  })
  
  
  # Genera la probabilidad de corte
  output$txtrfprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$rf[[nombre.modelo$x]]$prob 
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
  output$txtrfprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$rf[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$rf[[nombre.modelo$x]]$mc   <- results$MC
        modelos$rf[[nombre.modelo$x]]$pred <- results$Prediccion
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
  default.codigo.rf <- function(){
    # Se actualiza el código del modelo
    codigo <- rf.modelo(variable.pr = updateData$variable.predecir,
                        ntree = isolate(input$ntree.rf),
                        mtry = isolate(input$mtry.rf))
    cod  <- paste0("### rfl\n",codigo)
    
    # Se genera el código de la prediccion
    codigo <- codigo.prediccion("rf")
    cod    <- paste0(cod,codigo)
    
    # Se genera el código de la matriz
    codigo <- codigo.MC("rf")
    cod    <- paste0(cod,codigo)
    
    # Se genera el código de los indices
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.rf)\n")
    
    cod  <- paste0(cod,codigo)
    
    # Cambia el código del gráfico de rf
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}

## To be copied in the UI
# mod_r_forest_ui("r_forest_ui_1")

## To be copied in the server
# callModule(mod_r_forest_server, "r_forest_ui_1")

