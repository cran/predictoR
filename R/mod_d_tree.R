#' d_tree UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_d_tree_ui <- function(id){
  ns <- NS(id)
  
  
  opc_dt <- div(conditionalPanel(
    "input['d_tree_ui_1-BoxDt']   == 'tabDtModelo' || input['d_tree_ui_1-BoxDt'] == 'tabDtProb' || input['d_tree_ui_1-BoxDt'] == 'tabDtProbInd'",
    tabsOptions(heights = c(70), tabs.content = list(
      list(
        conditionalPanel(
          "input['d_tree_ui_1-BoxDt']   == 'tabDtModelo'",
          options.run(ns("runDt")), tags$hr(style = "margin-top: 0px;"),
          fluidRow(col_6(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 2, width = "100%",min = 1)),
                   col_6(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1))),
          fluidRow(col_12(selectInput(inputId = ns("split.dt"), label = labelInput("splitIndex"),selected = 1,
                                      choices =  list("gini" = "gini", "Entropia" = "information"))))),
        conditionalPanel(
          "input['d_tree_ui_1-BoxDt'] == 'tabDtProb'",
          options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
          div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                 choices =  "", width = "100%"))),
          div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                  width = "100%")))
        ),
        conditionalPanel(
          "input['d_tree_ui_1-BoxDt'] == 'tabDtProbInd'",
          options.run(ns("runProbInd")), tags$hr(style = "margin-top: 0px;"),
          div(col_12(selectInput(inputId = ns("cat_probC"),label = labelInput("selectCat"),
                                 choices =  "", width = "100%"))),
          div(col_12(numericInput(inputId = ns("val_probC"),label =  labelInput("probC"), value = 0.5, min = 0, max = 1, step = 0.1, 
                                  width = "100%")))
        )
        
      )))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxDt"), opciones = opc_dt,
      tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
               withLoader(verbatimTextOutput(ns("txtDt")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
               withLoader(plotOutput(ns('plot_dt'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabDtPred",
               withLoader(DT::dataTableOutput(ns("dtPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabDtMC",
               withLoader(plotOutput(ns('plot_dt_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtDtMC"))),
      
      tabPanel(title = labelInput("indices"),value = "tabDtIndex",
               fluidRow(col_6(echarts4rOutput(ns("dtPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("dtErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("dtIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("dtIndErrTable"))))),
      
      tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
               withLoader(verbatimTextOutput(ns("rulesDt")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probC"), value = "tabDtProbInd",
               withLoader(verbatimTextOutput(ns("txtdtprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabDtProb",
               withLoader(verbatimTextOutput(ns("txtdtprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}

#' d_tree Server Function
#'
#' @noRd 
mod_d_tree_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  
  
  observeEvent(updateData$datos, {
    modelos2$dt = list(n = 0, mcs = vector(mode = "list", length = 10))
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
    updateTabsetPanel(session, "BoxDt",selected = "tabDtModelo")
  })
  
  
  # Genera el texto del modelo, predicción y mc de DT
  output$txtDt <- renderPrint({
    input$runDt
    tryCatch({
      default.codigo.dt()
      train  <- updateData$datos.aprendizaje
      test   <- updateData$datos.prueba
      var    <- paste0(updateData$variable.predecir, "~.")
      tipo   <- isolate(input$split.dt)
      minsplit<-isolate(input$minsplit.dt)
      maxdepth<-isolate(input$maxdepth.dt)
      nombre <- paste0("dtl-",tipo)
      modelo <- traineR::train.rpart(as.formula(var), data = train,
                                     control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),parms = list(split = tipo))
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
        modelos$dt[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred , prob = prob, mc = mc)
        modelos2$dt$n <- modelos2$dt$n + 1
        modelos2$dt$mcs[modelos2$dt$n] <- general.indexes(mc = mc)
        if(modelos2$dt$n > 9)
          modelos2$dt$n <- 0
      
        })
      nombre.modelo$x <- nombre
      print(modelo)
    },error = function(e){
      return(invisible(""))
    })
  })
  
  #Tabla de la predicción
  output$dtPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$dt[[nombre.modelo$x]]$pred,idioma = idioma, test, var)
    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtDtMC    <- renderPrint({
    print(modelos$dt[[nombre.modelo$x]]$mc)
  })
  
  #Gráfico de la Matríz de Confusión
  output$plot_dt_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$dt[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$dtIndPrecTable <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.dt <- indices.generales(modelos$dt[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.dt,"dt", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$dtIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.dt <- indices.generales(modelos$dt[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$dtPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.dt[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$dtErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.dt[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    xtable(indices.error.table(indices.dt,"dt"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  #Plotear el árbol
  output$plot_dt <- renderPlot({
    tryCatch({
      tipo   <- isolate(input$split.dt)
      datos  <- updateData$datos
      var    <- updateData$variable.predecir
      num    <- length(levels(datos[,var]))
      modelo <- modelos$dt[[nombre.modelo$x]]$modelo
      
      # Cambia el código del gráfico del árbol
      codigo <- paste0("### garbol\n", dt.plot(tipo))
      isolate(codedioma$code <- append(codedioma$code, codigo))
      
      prp(modelo, type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
          fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
          box.col = gg_color_hue(num)[modelo$frame$yval], roundint=FALSE)
    },
    error = function(e){
      output$plot_dt <- renderPlot(NULL)
    })
  })
  
  #Mostrar Reglas
  output$rulesDt <- renderPrint({
    tipo  <- isolate(input$split.dt)
    model <- modelos$dt[[nombre.modelo$x]]$modelo
    var   <- model$prmdt$var.pred
    cod <- paste0("### reglas\n", paste0("rpart.rules(modelo.dt.",tipo,", cover = TRUE,nn = TRUE , style = 'tall', digits=3,
                            response.name ='",paste0("Rule Number - ", var),"')\n"))
    isolate(codedioma$code <- append(codedioma$code, cod))
    
    rpart.plot::rpart.rules(model, cover = TRUE,nn = TRUE ,roundint=FALSE, style = "tall", digits=3, 
                            response.name = paste0("Rule Number - ", var))
    
  })
  
  
  # Genera la probabilidad de corte
  output$txtdtprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$dt[[nombre.modelo$x]]$prob 
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
  output$txtdtprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$dt[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$dt[[nombre.modelo$x]]$mc   <- results$MC
        modelos$dt[[nombre.modelo$x]]$pred <- results$Prediccion
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
  default.codigo.dt <- function() {
    tipo   <- isolate(input$split.dt)
    codigo <- dt.modelo(variable.pr = updateData$variable.predecir,
                        minsplit = isolate(input$minsplit.dt),
                        maxdepth = isolate(input$maxdepth.dt),
                        split = tipo)
    cod  <- paste0("### dtl\n",codigo)
    
    # Se genera el código de la prediccion
    codigo <- codigo.prediccion("dt", tipo)
    cod    <- paste0(cod,codigo)
    
    # Se genera el código de la matriz
    codigo <- codigo.MC("dt",  tipo)
    cod    <- paste0(cod, codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo  <- paste0(codigo,"\nindices.generales(MC.dt.",tipo,")\n")
    cod  <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
}

## To be copied in the UI
# mod_d_tree_ui("d_tree_ui_1")

## To be copied in the server
# callModule(mod_d_tree_server, "d_tree_ui_1")

