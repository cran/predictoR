#' xgboosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom shiny NS tagList 
mod_xgboosting_ui <- function(id){
  ns <- NS(id)
  
  opc_xgb <- div(conditionalPanel(
    "input['xgboosting_ui_1-BoxXgb']  == 'tabXgbModelo' || input['xgboosting_ui_1-BoxXgb'] == 'tabXgbProb' || input['xgboosting_ui_1-BoxXgb'] == 'tabXgbProbInd'",
    tabsOptions(heights = c(70), tabs.content = list(
      list(
        conditionalPanel(
          "input['xgboosting_ui_1-BoxXgb']  == 'tabXgbModelo' ",
          options.run(ns("runXgb")), tags$hr(style = "margin-top: 0px;"),
          fluidRow(col_12(selectInput(inputId = ns("boosterXgb"), label = labelInput("selbooster"),selected = 1,
                                      choices = c("gbtree", "gblinear", "dart")))),
          fluidRow(col_6(numericInput(ns("maxdepthXgb"), labelInput("maxdepth"), min = 1,step = 1, value = 6)),
                   col_6(numericInput(ns("nroundsXgb"), labelInput("selnrounds"), min = 0,step = 1, value = 50)))),
        conditionalPanel(
          "input['xgboosting_ui_1-BoxXgb'] == 'tabXgbProb'",
          options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
          div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                 choices =  "", width = "100%"))),
          div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                  width = "100%")))
        ),
        conditionalPanel(
          "input['xgboosting_ui_1-BoxXgb'] == 'tabXgbProbInd'",
          options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
          div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                 choices =  "", width = "100%"))),
          div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                  width = "100%")))
        )
      )))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxXgb"), opciones = opc_xgb,
      tabPanel(title = labelInput("generatem"), value = "tabXgbModelo",
               withLoader(verbatimTextOutput(ns("txtxgb")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("varImp"), value = "tabXgbImp",
               withLoader(echarts4rOutput(ns('plot_xgb'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabXgbPred",
               withLoader(DT::dataTableOutput(ns("xgbPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabXgbMC",
               withLoader(plotOutput(ns('plot_xgb_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtxgbMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabXgbIndex",
               fluidRow(col_6(echarts4rOutput(ns("xgbPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("xgbErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("xgbIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("xgbIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabXgbProbInd",
               withLoader(verbatimTextOutput(ns("txtxgbprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabXgbProb",
               withLoader(verbatimTextOutput(ns("txtxgbprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}

#' xgboosting Server Function
#'
#' @noRd 
mod_xgboosting_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  observeEvent(updateData$datos, {
    modelos2$xgb = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  # When load training-testing
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
    updateTabsetPanel(session, "BoxXgb",selected = "tabXgbModelo")
  })
  
  # Update model text
  output$txtxgb <- renderPrint({
    input$runXgb
    tryCatch({
      default.codigo.xgb()
      train    <- updateData$datos.aprendizaje
      test     <- updateData$datos.prueba
      var      <- paste0(updateData$variable.predecir, "~.")
      tipo     <- isolate(input$boosterXgb)
      max.depth<- isolate(input$maxdepthXgb)
      n.rounds <- isolate(input$nroundsXgb)
      
      nombre <- paste0("xgb-",tipo)
      modelo <- traineR::train.xgboost(as.formula(var), data = train, booster = tipo, 
                                       max_depth = max.depth, nrounds = n.rounds)
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
        modelos$xgb[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc)
        modelos2$xgb$n <- modelos2$xgb$n + 1
        modelos2$xgb$mcs[modelos2$xgb$n] <- general.indexes(mc = mc)
        if(modelos2$xgb$n > 9)
          modelos2$xgb$n <- 0
        })
      nombre.modelo$x <- nombre
      print(modelo)
    },error = function(e){
      return(invisible(""))
    })
  })
  
  # Update predict table
  output$xgbPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$xgb[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  # Update confusion matrix text
  output$txtxgbMC    <- renderPrint({
    print(modelos$xgb[[nombre.modelo$x]]$mc)
  })
  
  # Update confusion matrix plot
  output$plot_xgb_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$xgb[[nombre.modelo$x]]$mc)
  })
  
  # Update indexes table
  output$xgbIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.xgb <- indices.generales(modelos$xgb[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.xgb,"xgb", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update error table
  output$xgbIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.xgb <- indices.generales(modelos$xgb[[nombre.modelo$x]]$mc)
    # Overall accuracy and overall error plot
    output$xgbPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.xgb[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$xgbErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.xgb[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.xgb,"xgb"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Importance plot
  output$plot_xgb <- renderEcharts4r({
    tryCatch({
      modelo                    <- modelos$xgb[[nombre.modelo$x]]$modelo
      nombres                   <- modelo$feature_names    
      variables.importantes     <- xgboost::xgb.importance(feature_names = nombres, model = modelo ) 
      variables.importantes     <- variables.importantes[1:length(nombres),]  
      variables.importantes[,2] <- abs(variables.importantes[,2]) 
      variables.importantes     <- na.omit(variables.importantes) 
      label                     <- variables.importantes$Feature
      values                    <- variables.importantes[,2]
      color                     <- gg_color_hue(length(label))
      datos.xgb <- data.frame(label  = label, 
                              values = values, 
                              color  = color) 
      datos.xgb |>  e_charts(label) |>  e_bar(values, name = var) |>  
        e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|>
        e_add_nested("itemStyle", color)|>  
        e_flip_coords()|>  
        e_y_axis(inverse = TRUE)
    }, error = function(e) {
      showNotification(paste0("Error :",e), duration = 15, type = "error")
      
      return(NULL)
    })
  })
  
  # Genera la probabilidad de corte
  output$txtxgbprob <- renderPrint({
    input$runProb 
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$xgb[[nombre.modelo$x]]$prob 
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
  output$txtxgbprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$xgb[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$xgb[[nombre.modelo$x]]$mc   <- results$MC
        modelos$xgb[[nombre.modelo$x]]$pred <- results$Prediccion
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
  
  # Update default code
  default.codigo.xgb <- function() {
    tipo   <- isolate(input$boosterXgb)
    
    #Modelo
    codigo <- xgb.modelo(updateData$variable.predecir,
                         booster   = tipo,
                         max.depth = isolate(input$maxdepthXgb),
                         n.rounds  = isolate(input$nroundsXgb))
    cod  <- paste0("### xgb\n",codigo)
    
    
    # Prediccion
    codigo <- codigo.prediccion("xgb",  tipo)
    cod    <- paste0(cod,codigo)
    
    # Matriz de Confusion
    codigo <- codigo.MC("xgb",  tipo)
    cod    <- paste0(cod,codigo)
    
    #Indices Generales
    codigo <- extract.code("indices.generales")
    codigo <- paste0(codigo,"\nindices.generales(MC.xgb.",tipo,")\n")
    cod  <- paste0(cod,codigo)
    
    #Código de importancia de variables
    
    cod  <- paste0(cod,"### docImpV\n", e_xgb_varImp(booster = tipo))
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}

## To be copied in the UI
# mod_xgboosting_ui("xgboosting_ui_1")

## To be copied in the server
# callModule(mod_xgboosting_server, "xgboosting_ui_1")

