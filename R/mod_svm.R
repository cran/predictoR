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
  codigo.svm <- list(conditionalPanel("input['svm_ui_1-BoxSvm'] == 'tabSvmModelo'",
                                      codigo.monokai(ns("fieldCodeSvm"),height = "10vh")),
                     conditionalPanel("input['svm_ui_1-BoxSvm']  == 'tabSvmPlot'",
                                      codigo.monokai(ns("fieldCodeSvmPlot"),height = "10vh")),
                     conditionalPanel("input['svm_ui_1-BoxSvm']  == 'tabSvmPred'",
                                      codigo.monokai(ns("fieldCodeSvmPred"),height = "10vh")),
                     conditionalPanel("input['svm_ui_1-BoxSvm']  == 'tabSvmMC'",
                                      codigo.monokai(ns("fieldCodeSvmMC"),height = "10vh")),
                     conditionalPanel("input['svm_ui_1-BoxSvm']  == 'tabSvmIndex'",
                                      codigo.monokai(ns("fieldCodeSvmIG"),height = "10vh")))
  
  codigo.svm.run <- list(conditionalPanel("input['svm_ui_1-BoxSvm'] == 'tabSvmModelo'",
                                      codigo.monokai(ns("fieldCodeSvm"),height = "10vh")),
                     conditionalPanel("input['svm_ui_1-BoxSvm']  == 'tabSvmPlot'",
                                      codigo.monokai(ns("fieldCodeSvmPlot"),height = "10vh")))
  
  opc_svm <-     div(
    conditionalPanel(
      "input['svm_ui_1-BoxSvm']   == 'tabSvmModelo' || input['svm_ui_1-BoxSvm']  == 'tabSvmPlot'",
      tabsOptions(heights = c(70, 30), tabs.content = list(
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
                           options = list(maxItems = 2, placeholder = ""), width = "100%"))),
        codigo.svm.run
      ))),
    conditionalPanel(
      "input['svm_ui_1-BoxSvm']   != 'tabSvmModelo' && input['svm_ui_1-BoxSvm']  != 'tabSvmPlot'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        codigo.svm
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
               fluidRow(col_12(shiny::tableOutput(ns("svmIndErrTable")))))
    )
  )
}
    
#' svm Server Function
#'
#' @noRd 
mod_svm_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  nombre.modelo <- rv(x = NULL)
  
  # When load training-testing
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    nombres <- colnames.empty(var.numericas(updateData$datos))
    updateSelectizeInput(session, "select_var_svm_plot", choices = nombres)
    updateTabsetPanel(session, "BoxSvm",selected = "tabSvmModelo")
    default.codigo.svm()
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
    pred   <- predict(modelo , test, type = 'class')
    prob   <- predict(modelo , test, type = 'prob')
    mc     <- confusion.matrix(test, pred)
    isolate(modelos$svm[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred ,prob = prob , mc = mc))
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
    idioma <- updateData$idioma
    obj.predic(modelos$svm[[nombre.modelo$x]]$pred,idioma = idioma, test, var)  
    },server = FALSE)
  
  # Update confusion matrix text
  output$txtSvmMC    <- renderPrint({
    print(modelos$svm[[nombre.modelo$x]]$mc)
  })
  
  # Update confusion matrix plot
  output$plot_svm_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$svm[[nombre.modelo$x]]$mc)
  })
  
  # Update indexes table
  output$svmIndPrecTable <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.svm <- indices.generales(modelos$svm[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.svm,"SVM", idioma = idioma))
    }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  # Update error table
  output$svmIndErrTable  <- shiny::renderTable({
    idioma <- updateData$idioma
    indices.svm <- indices.generales(modelos$svm[[nombre.modelo$x]]$mc)
    # Overall accuracy and overall error plot
    output$svmPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.svm[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$svmErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.svm[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.svm,"SVM"))
  
    }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Update default code
  default.codigo.svm <- function() {
    kernel <- isolate(input$kernel.svm)
    # Se actualiza el código del modelo
    codigo <- svm.modelo(variable.pr = updateData$variable.predecir,
                         scale = isolate(input$switch.scale.svm),
                         kernel = kernel)
    
    updateAceEditor(session, "fieldCodeSvm", value = codigo)

    # Se genera el código de la predicción
    codigo <- svm.prediccion(kernel)
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- svm.MC(kernel)
    updateAceEditor(session, "fieldCodeSvmMC", value = codigo)

    # Se genera el código de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)

  }
  
  # Update SVM plot
  output$plot_svm <- renderPlot({
    tryCatch({
      idioma    <- updateData$idioma
      train     <- updateData$datos.aprendizaje
      datos     <- isolate(updateData$datos)
      variable  <- isolate(updateData$variable.predecir)
      variables <- input$select_var_svm_plot
      var       <- paste0(isolate(updateData$variable.predecir), "~",paste(variables, collapse = "+") )
      var2      <- paste(variables, collapse = "~") 
      k         <- isolate(input$kernel.svm)
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(variable, train, variables, colnames(datos[, -which(colnames(datos) == variable)]), k))
      if (length(variables) == 2){
      modelo.svm.temp <- traineR::train.svm(as.formula(var) , data = train, kernel = k) 
      slices <- lapply(1:(ncol(datos)-1),function(i) i)
      names(slices) <- colnames(datos[, -which(colnames(datos) == variable)])
      plot(modelo.svm.temp, datos, as.formula(var2), slice = slices)
      }else{
        return(NULL)

      }
      },error = function(e){
        showNotification(e,
                         duration = 10,
                         type = "error")
        return(NULL)
      })
  })
}
    
## To be copied in the UI
# mod_svm_ui("svm_ui_1")
    
## To be copied in the server
# callModule(mod_svm_server, "svm_ui_1")
 
