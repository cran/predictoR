#' svm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_svm_ui <- function(id){
  ns <- NS(id)
  
  opc_svm <-     div(
    conditionalPanel(
      "input['svm_ui_1-BoxSvm']   == 'tabSvmModelo' || input['svm_ui_1-BoxSvm']  == 'tabSvmPlot' || input['svm_ui_1-BoxSvm'] == 'tabSvmProb' || input['svm_ui_1-BoxSvm'] == 'tabSvmProbInd'",
      tabsOptions(heights = c(70), tabs.content = list(
        list(
          conditionalPanel(
            "input['svm_ui_1-BoxSvm']   == 'tabSvmModelo'",
            options.run(ns("runSvm")), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_6(
              radioSwitch(ns("switch.scale.svm"), "escal", c("si", "no"))),
              col_6(
                selectInput(inputId = ns("kernel.svm"), label = labelInput("selkernel"), selected = "radial",
                            choices =  c("linear", "polynomial", "radial", "sigmoid"))))),
          conditionalPanel(
            "input['svm_ui_1-BoxSvm']  == 'tabSvmPlot'",
            options.base(), tags$hr(style = "margin-top: 0px;"),
            selectizeInput(ns("select_var_svm_plot"),NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                           options = list(maxItems = 2, placeholder = ""), width = "100%")),
          conditionalPanel(
            "input['svm_ui_1-BoxSvm'] == 'tabSvmProb'",
            options.run(ns("runProb")), tags$hr(style = "margin-top: 0px;"),
            div(col_12(selectInput(inputId = ns("cat.sel.prob"),label = labelInput("selectCat"),
                                   choices =  "", width = "100%"))),
            div(col_12(numericInput(inputId = ns("by.prob"),label =  labelInput("selpaso"), value = -0.05, min = -0.0, max = 1,
                                    width = "100%")))
          ),
          conditionalPanel(
            "input['svm_ui_1-BoxSvm'] == 'tabSvmProbInd'",
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
      id = ns("BoxSvm"), opciones = opc_svm,
      tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
               withLoader(verbatimTextOutput(ns("txtSvm")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gclasificacion"), value = "tabSvmPlot",
               withLoader(plotOutput(ns('plot_svm'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabSvmPred",
               withLoader(DT::dataTableOutput(ns("svmPrediTable")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabSvmMC",
               withLoader(plotOutput(ns('plot_svm_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtSvmMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
               fluidRow(col_6(echarts4rOutput(ns("svmPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("svmErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("svmIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("svmIndErrTable"))))),
      tabPanel(title = labelInput("probC"), value = "tabSvmProbInd",
               withLoader(verbatimTextOutput(ns("txtsvmprobInd")), 
                          type = "html", loader = "loader4")),
      tabPanel(title = labelInput("probCstep"), value = "tabSvmProb",
               withLoader(verbatimTextOutput(ns("txtsvmprob")), 
                          type = "html", loader = "loader4"))
    )
  )
}

#' svm Server Function
#'
#' @noRd 
mod_svm_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombre.modelo <- rv(x = NULL)
  
  
  observeEvent(updateData$datos, {
    modelos2$svm = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  
  # When load training-testing
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
    updateSelectizeInput(session, "select_var_svm_plot", choices = nombres)
    updateTabsetPanel(session, "BoxSvm",selected = "tabSvmModelo")
  })
  
  # Update model text
  output$txtSvm <- renderPrint({
    input$runSvm
    default.codigo.svm()
    tryCatch({
      train  <- updateData$datos.aprendizaje
      test   <- updateData$datos.prueba
      var    <- paste0(updateData$variable.predecir, "~.")
      scales <- isolate(input$switch.scale.svm)
      k      <- isolate(input$kernel.svm)
      nombre <- paste0("svml-",k)
      modelo <- traineR::train.svm(as.formula(var), data = train, scale = as.logical(scales), kernel = k)
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
      
      
      isolate({modelos$svm[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc)
      modelos2$svm$n <- modelos2$svm$n + 1
      modelos2$svm$mcs[modelos2$svm$n] <- general.indexes(mc = mc)
      if(modelos2$svm$n > 9)
        modelos2$svm$n <- 0
      })
      nombre.modelo$x <- nombre
      print(modelo)
    },error = function(e){
      return(invisible(""))
    })
  })
  
  # Update predict table
  output$svmPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- codedioma$idioma
    obj.predic(modelos$svm[[nombre.modelo$x]]$pred,idioma = idioma, test, var)  
  },server = FALSE)
  
  # Update confusion matrix text
  output$txtSvmMC    <- renderPrint({
    print(modelos$svm[[nombre.modelo$x]]$mc)
  })
  
  # Update confusion matrix plot
  output$plot_svm_mc <- renderPlot({
    idioma <- codedioma$idioma
    exe(plot_MC_code(idioma = idioma))
    plot.MC(modelos$svm[[nombre.modelo$x]]$mc)
  })
  
  # Update indexes table
  output$svmIndPrecTable <- shiny::renderTable({
    idioma      <- codedioma$idioma
    indices.svm <- indices.generales(modelos$svm[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.svm,"SVM", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update error table
  output$svmIndErrTable  <- shiny::renderTable({
    idioma <- codedioma$idioma
    indices.svm <- indices.generales(modelos$svm[[nombre.modelo$x]]$mc)
    # Overall accuracy and overall error plot
    output$svmPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.svm[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$svmErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.svm[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.svm,"SVM"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Genera la probabilidad de corte
  output$txtsvmprob <- renderPrint({
    input$runProb
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat.sel.prob)
      paso       <- isolate(input$by.prob)
      prediccion <- modelos$svm[[nombre.modelo$x]]$prob 
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
  output$txtsvmprobInd <- renderPrint({
    input$runProbInd
    tryCatch({
      test       <- updateData$datos.prueba
      variable   <- updateData$variable.predecir
      choices    <- levels(test[, variable])
      category   <- isolate(input$cat_probC)
      corte      <- isolate(input$val_probC)
      prediccion <- modelos$svm[[nombre.modelo$x]]$prob 
      Score      <- prediccion$prediction[,category]
      Clase      <- test[,variable]
      if(!is.null(Score) & length(choices) == 2){
        results <- prob.values.ind(Score, Clase, choices, category, corte)
        modelos$svm[[nombre.modelo$x]]$mc   <- results$MC
        modelos$svm[[nombre.modelo$x]]$pred <- results$Prediccion
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

  # Update SVM plot
  output$plot_svm <- renderPlot({
    tryCatch({
      idioma    <- codedioma$idioma
      train     <- updateData$datos.aprendizaje
      datos     <- isolate(updateData$datos)
      variable  <- isolate(updateData$variable.predecir)
      variables <- input$select_var_svm_plot
      var       <- paste0(isolate(updateData$variable.predecir), "~",paste(variables, collapse = "+") )
      var2      <- paste(variables, collapse = "~") 
      k         <- isolate(input$kernel.svm)
      cod       <- svm.plot(variable, train, variables, colnames(datos[, -which(colnames(datos) == variable)]), k)
      cod       <- paste0("### gclasificacion\n",cod)
      
      if (length(variables) == 2){
        isolate(codedioma$code <- append(codedioma$code, cod))
        modelo.svm.temp        <- traineR::train.svm(as.formula(var) , data = train, kernel = k) 
        slices <- lapply(1:(ncol(datos)-1),function(i) i)
        names(slices) <- colnames(datos[, -which(colnames(datos) == variable)])
        plot(modelo.svm.temp, datos, as.formula(var2), slice = slices)
      }
      else{
        return(NULL)
      }
    },error = function(e){
      showNotification(e,
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })
  
  
  # Update default code
  default.codigo.svm <- function() {
    kernel <- isolate(input$kernel.svm)
    # Se actualiza el código del modelo
    codigo <- svm.modelo(variable.pr = updateData$variable.predecir,
                         scale = isolate(input$switch.scale.svm),
                         kernel = kernel)
    
    cod  <- paste0("### svml\n",codigo)
    
    # Se genera el código de la prediccion
    codigo       <- codigo.prediccion("svm",  kernel)
    cod  <- paste0(cod,codigo)
    
    # Se genera el código de la matriz
    codigo       <- codigo.MC("svm",  kernel)
    cod  <- paste0(cod,codigo)
    
    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    codigo <- paste0(codigo,"\nindices.generales(MC.svm.",kernel,")\n")
    cod    <- paste0(cod,codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
    
  }
  
}

## To be copied in the UI
# mod_svm_ui("svm_ui_1")

## To be copied in the server
# callModule(mod_svm_server, "svm_ui_1")

