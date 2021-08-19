#' penalized_l_r UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penalized_l_r_ui <- function(id){
  ns <- NS(id)
  
  codigo.rlr.run<- list(conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'",
                                         codigo.monokai(ns("fieldCodeRlrLanda"), height = "10vh")),
                        conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo'",
                                         codigo.monokai(ns("fieldCodeRlr"), height = "10vh")),
                        conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrBetas'",
                                         codigo.monokai(ns("fieldCodeRlrBetas"), height = "10vh")))
  
  codigo.rlr  <- list(conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrPosibLanda'",
                                       codigo.monokai(ns("fieldCodeRlrPosibLanda"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrPred'",
                                       codigo.monokai(ns("fieldCodeRlrPred"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrMC'",
                                       codigo.monokai(ns("fieldCodeRlrMC"), height = "10vh")),
                      conditionalPanel("input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrIndex'",
                                       codigo.monokai(ns("fieldCodeRlrIG"), height = "10vh")))
  
  opc_rlr  <-   div(
    conditionalPanel(
      "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo' || input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda' || input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrBetas'",
      tabsOptions(heights = c(70, 30), tabs.content = list(
        list(
          conditionalPanel(
            "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrModelo'",
            options.run(ns("runRlr")), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_6(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                       choices = list("Ridge" = 0, "Lasso" = 1))),
                     col_6(radioSwitch(ns("switch.scale.rlr"), "escal", c("si", "no"))))),
          conditionalPanel(
            "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'  || input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrBetas'",
            options.base(), tags$hr(style = "margin-top: 0px;"),
            fluidRow(col_12(selectInput(inputId = ns("coeff.sel"),label = labelInput("selectCat"),
                                        choices =  "", width = "100%"))),
            conditionalPanel(
              "input['penalized_l_r_ui_1-BoxRlr'] == 'tabRlrLanda'" ,
              fluidRow(col_6(id = ns("colManualLanda"),br(),
                           numericInput(ns("landa"), labelInput("landa"),value = 0, width = "100%")), br(),
                     col_6(radioSwitch(ns("permitir.landa"), "", c("manual", "automatico"), val.def = F)))))),
        codigo.rlr.run
      ))),
    conditionalPanel(
      "input['penalized_l_r_ui_1-BoxRlr'] != 'tabRlrModelo' && input['penalized_l_r_ui_1-BoxRlr'] != 'tabRlrLanda' && input['penalized_l_r_ui_1-BoxRlr'] != 'tabRlrBetas'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        codigo.rlr
      )))
  )
  tagList(
    tabBoxPrmdt(
      id = ns("BoxRlr"), opciones = opc_rlr,
      tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
               withLoader(verbatimTextOutput(ns("txtRlr")), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
               withLoader(echarts4rOutput(ns('plot_rlr_posiblanda'), height = "55vh"), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("gcoeff"),value = "tabRlrLanda",
               withLoader(echarts4rOutput(ns('plot_rlr_landa'), height = "55vh"), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("betas"),value = "tabRlrBetas",
               withLoader(verbatimTextOutput(ns('txtBetas')), 
                          type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("predm"), value = "tabRlrPred",
               withLoader(DT::dataTableOutput(ns("rlrPrediTable")), 
               type = "html", loader = "loader4")),
      
      tabPanel(title = labelInput("mc"), value = "tabRlrMC",
               withLoader(plotOutput(ns('plot_rlr_mc'), height = "45vh"), 
                          type = "html", loader = "loader4"),
               verbatimTextOutput(ns("txtrlrMC"))),
      
      tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
               fluidRow(col_6(echarts4rOutput(ns("rlrPrecGlob"), width = "100%")),
                        col_6(echarts4rOutput(ns("rlrErrorGlob"), width = "100%"))),
               fluidRow(col_12(shiny::tableOutput(ns("rlrIndPrecTable")))),
               fluidRow(col_12(shiny::tableOutput(ns("rlrIndErrTable")))))
    )
  )
}
    
#' penalized_l_r Server Function
#'
#' @noRd 
mod_penalized_l_r_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  nombre.modelo <- rv(x = NULL)
  cv            <- rv(cv.glm = NULL)
  
  #Cuando se generan los datos de prueba y aprendizaje
  observeEvent(c(updateData$datos.aprendizaje,updateData$datos.prueba), {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    choices      <- unique(datos[, variable])
    updateSelectInput(session, "coeff.sel", choices = choices, selected = choices[1])
    updateTabsetPanel(session, "BoxRlr",selected = "tabRlrModelo")
    default.codigo.rlr()
  })
  
  # Genera el texto del modelo, predicción y mc de RLR
  output$txtRlr <- renderPrint({
    input$runRlr
    tryCatch({
    default.codigo.rlr()
    train  <- updateData$datos.aprendizaje
    test   <- updateData$datos.prueba
    var    <- paste0(updateData$variable.predecir, "~.")
    scales <- isolate(input$switch.scale.rlr)
    tipo   <- rlr.type()
    alpha  <- isolate(input$alpha.rlr)
    nombre <- paste0("rlr-",tipo)
    modelo <- traineR::train.glmnet(as.formula(var), data = train, standardize = as.logical(scales), alpha = alpha, family = 'multinomial' )
    prob   <- predict(modelo , test, type = 'prob')
    nombre.modelo$x <- nombre
    x         <- model.matrix(as.formula(var), train)[, -1]
    y         <- train[,updateData$variable.predecir]
    cv$cv.glm <- glmnet::cv.glmnet(x, y, standardize = as.logical(scales), alpha = alpha ,family = 'multinomial')
    pred      <- predict(modelo , test, type = 'class', s = mean(c(cv$cv.glm$lambda.min, cv$cv.glm$lambda.1se)))
    mc        <- confusion.matrix(test, pred)
    updateNumericInput(session, 
                       "landa", 
                       max   =  round(max(log(modelo$lambda)), 5), 
                       min   =  round(min(log(modelo$lambda)), 5),
                       value =  round(log(mean(c(cv$cv.glm$lambda.min, cv$cv.glm$lambda.1se))),5))
    isolate(modelos$rlr[[nombre]] <- list(nombre = nombre, modelo = modelo ,pred = pred, prob = prob , mc = mc))
    print(modelo)
  },error = function(e){
    return(invisible(""))
  })
  })
  #Tabla de la predicción
  output$rlrPrediTable <- DT::renderDataTable({
    test   <- updateData$datos.prueba
    var    <- updateData$variable.predecir
    idioma <- updateData$idioma
    obj.predic(modelos$rlr[[nombre.modelo$x]]$pred,idioma = idioma, test, var)    
  },server = FALSE)
  
  #Texto de la Matríz de Confusión
  output$txtrlrMC    <- renderPrint({
    print(modelos$rlr[[nombre.modelo$x]]$mc)
  })

  #Gráfico de la Matríz de Confusión
  output$plot_rlr_mc <- renderPlot({
    idioma <- updateData$idioma
    exe(plot.MC.code(idioma = idioma))
    plot.MC(modelos$rlr[[nombre.modelo$x]]$mc)
  })
  
  #Tabla de Indices por Categoría 
  output$rlrIndPrecTable <- shiny::renderTable({
    idioma      <- updateData$idioma
    indices.rlr <- indices.generales(modelos$rlr[[nombre.modelo$x]]$mc)
    
    xtable(indices.prec.table(indices.rlr,"rlr", idioma = idioma))
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  
  #Tabla de Errores por Categoría
  output$rlrIndErrTable  <- shiny::renderTable({
    idioma      <- updateData$idioma
    indices.rlr <- indices.generales(modelos$rlr[[nombre.modelo$x]]$mc)
    #Gráfico de Error y Precisión Global
    output$rlrPrecGlob  <-  renderEcharts4r(e_global_gauge(round(indices.rlr[[1]],2), tr("precG",idioma), "#B5E391", "#90C468"))
    output$rlrErrorGlob <-  renderEcharts4r(e_global_gauge(round(indices.rlr[[2]],2), tr("errG",idioma),  "#E39191", "#C46868"))
    
    xtable(indices.error.table(indices.rlr,"rlr"))
    
  }, spacing = "xs",bordered = T, width = "100%", align = "c", digits = 2)
  
  # Habilita o deshabilita la semilla
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      modelo <- modelos$rlr[[nombre.modelo$x]]$modelo
      updateNumericInput(session, 
                         "landa", 
                         max   =  round(max(log(modelo$lambda)), 5), 
                         min   =  round(min(log(modelo$lambda)), 5))
      shinyjs::enable("landa")
    } else {
      tryCatch({
      updateNumericInput(session, 
                         "landa", 
                          value =  round(log(mean(c(cv$cv.glm$lambda.min, cv$cv.glm$lambda.1se))),5))},
      warning = function(w){})
      shinyjs::disable("landa")
    }
  })

  #Obtiene el lambda seleccionado
  get_lambda_rlr <- function(){
    landa  <- NULL
    modelo <- modelos$rlr[[nombre.modelo$x]]$modelo
    idioma <- updateData$idioma
    if (!is.na(input$landa) && (input$permitir.landa=="TRUE")) {
        if(input$landa <= round(max(log(modelo$lambda)), 5) && input$landa >= round(min(log(modelo$lambda)), 5)){
           landa <- input$landa
        }
        else{
          landa <- round(log(mean(c(cv$cv.glm$lambda.min, cv$cv.glm$lambda.1se))),5)
          updateNumericInput(session,
                             "landa",
                             value = landa)
          showNotification(tr("limitLambda",idioma), duration = 10, type = "message")
        }
      pred   <- predict(modelo , updateData$datos.prueba, type = 'class', s = exp(landa) )
      mc     <- confusion.matrix(updateData$datos.prueba, pred)
      
      isolate(modelos$rlr[[nombre.modelo$x]]$pred <- pred)
      isolate(modelos$rlr[[nombre.modelo$x]]$mc   <- mc)
    }
    return(landa)
  }
  
  
  #Texto de los Betas
  output$txtBetas    <- renderPrint({
    category <- input$coeff.sel
    modelo   <- modelos$rlr[[nombre.modelo$x]]$modelo
    tipo     <- rlr.type()
    lambda   <- input$landa
    cv.glm   <- cv$cv.glm
    lambda   <- ifelse(is.null(lambda), round(log(mean(c(cv.glm$lambda.min, cv.glm$lambda.1se))),5), lambda)
    pos      <- select.beta(modelo, lambda)
    updateAceEditor(session, "fieldCodeRlrBetas", value = paste0("modelo.rlr.",tipo,"$beta[['",category,"']][,",pos,"]"))
    print(modelo$beta[[category]][,pos])
  })
  
  # Actualiza el código a la versión por defecto
  default.codigo.rlr <- function(){
    tipo  <- rlr.type()


    # Se actualiza el código del modelo
    codigo <- rlr.modelo(variable.pr = updateData$variable.predecir,
                         type        = tipo,
                         isolate(input$alpha.rlr),
                         isolate(input$switch.scale.rlr))
    
    updateAceEditor(session, "fieldCodeRlr", value = codigo)

    # Se genera el código del posible lambda
    codigo <- select.landa(updateData$variable.predecir,
                           isolate(input$alpha.rlr),
                           isolate(input$switch.scale.rlr),
                           tipo)

    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)

    # Se genera el código de la predicción
    codigo <- rlr.prediccion(tipo)
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)

    # Se genera el código de la matriz
    codigo <- rlr.MC(tipo)
    updateAceEditor(session, "fieldCodeRlrMC", value = codigo)

    # Se genera el código de los indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
  }
  
  #Gráfica de los Lambdas
  output$plot_rlr_posiblanda <- renderEcharts4r({
    idioma <- updateData$idioma
    tryCatch({  
      e_posib_lambda(cv$cv.glm, labels = c(tr("superior", idioma),tr("inferior", idioma),tr("lambda", idioma)))
    },
    error = function(e) { 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
      return(NULL)
    })

  })
  
  #Gráfica de los coeficientes Lambdas
  output$plot_rlr_landa <- renderEcharts4r({
    tryCatch({  
      lambda <- get_lambda_rlr()
      tipo   <- rlr.type()
      coeff  <- input$coeff.sel
      cv.glm <- cv$cv.glm
      modelo <- modelos$rlr[[nombre.modelo$x]]$modelo
      lambda <- ifelse(is.null(lambda), round(log(mean(c(cv.glm$lambda.min, cv.glm$lambda.1se))),5), lambda)
      updateAceEditor(session, "fieldCodeRlrLanda", value = paste0("e_coeff_landa(modelo.rlr.",tipo,", '",coeff,"', ",lambda,")"))
      e_coeff_landa(modelo, coeff, lambda, tr("lambda", updateData$idioma))
    },
    error = function(e){ 
      showNotification(paste0("Error (R/L) : ", e), duration = 15, type = "error")
    })
  })

  #Obtiene el algortimo a utilizar
  rlr.type <- function(){
    ifelse(isolate(input$alpha.rlr) == 0, "ridge", "lasso")
  }
}
    
## To be copied in the UI
# mod_penalized_l_r_ui("penalized_l_r_ui_1")
    
## To be copied in the server
# callModule(mod_penalized_l_r_server, "penalized_l_r_ui_1", updateData)
 
