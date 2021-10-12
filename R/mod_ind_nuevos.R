#' ind_nuevos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ind_nuevos_ui <- function(id){
  ns <- NS(id)
  muestra.datos.pred  <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred2'))), 
                             type = "html", loader = "loader4")  
  
  muestra.datos.pred3 <- box(title = labelInput("data"), status = "primary", width = 12, 
                             solidHeader = TRUE, collapsible = TRUE,
                             withLoader(DT::dataTableOutput(ns('contentsPred3'))), 
                             type = "html", loader = "loader4")

  cod_modelos         <- list(conditionalPanel("input.BoxModelo == 'crearModelo'",
                                      codigo.monokai(ns("fieldPredNuevos"),height = "10vh")),
                              conditionalPanel("input.BoxModelo == 'crearModelo'",
                                      codigo.monokai(ns("fieldCodePredPN"),height = "10vh")))
 
  tabs.modelos   <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                                tabs.content = list(list(codigo.monokai(ns("fieldPredNuevos"),height = "10vh"))))
  
  tabs.modelos2  <- div(tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                                tabs.content = list(list(codigo.monokai(ns("fieldCodePredPN"),height = "10vh")))))
  
  tagList(
    div(id = ns("primera"),
        fluidRow(
          col_11(
            tabBoxPrmdt(
            id = "BoxModeloq",
            tabPanel(
            title = p(labelInput("cargarComp"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
            collapsible = FALSE, collapsed = FALSE, value = "Cargar",
            fluidRow(
              col_5(
                    checkboxInput(ns('headerNPred'), labelInput("header"), value = T),
                    checkboxInput(ns('rownameNPred'), labelInput("Rownames"), value = T),
                    radioButtons(
                      ns('sepNPred'), labelInput("separador"), inline = T,
                      choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
                    ),
                    radioButtons(ns('decNPred'), labelInput("separadordec"), c(',', '.'), inline = T),
                    radioSwitch(ns("deleteNAnPred"), "eliminana", c("eliminar", "imputar")),
                    fileInput(
                      ns('archivoNPred'), labelInput("cargarchivo"), width = "100%",
                      placeholder = "", buttonLabel = labelInput("subir"),
                      accept = c('text/csv', '.csv', '.txt')), hr(),
                    actionButton(ns("loadButtonNPred"), labelInput("cargar"), width = "100%"), hr()),
              col_7(br(),muestra.datos.pred)))
            )),
          col_1(
            actionButton(ns("cargarnext"), label = NULL, width = "100%",
                         icon = icon("forward"))
          )
        )
      ),
    div(id = ns("segundo"),
        style = "display:none",
        fluidRow(
          col_1(actionButton (ns("transback"), label = NULL, width = "100%",
                              icon = icon("backward"))),
          col_10(
            tabBoxPrmdt(
            id = "BoxModeloe",
            tabPanel(
              title = p(labelInput("trans"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
              collapsible = FALSE, collapsed = FALSE, value = "Trasformar",
              fluidRow(
                col_5(
                      uiOutput(ns('transDataPredN')), hr(), 
                      actionButton(ns('transButton'), labelInput("aplicar"), width = "100%"), hr()
                ),
                col_7(br(),muestra.datos.pred2)), hr())
            )),
          col_1(actionButton(ns("transnext"), label = NULL, width = "100%",
                             icon = icon("forward")))
        )
    ),
    div(id = ns("tercera"),
        style = "display:none",
        fluidRow(col_1(actionButton(ns("modelback"), label = NULL, width = "100%",
                                          icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModeloa",opciones = conditionalPanel("input.BoxModelo == 'predicModelo'", tabs.modelos),
                     tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag") ,solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                                fluidRow(
                                  col_6(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%")),
                                  col_6(selectInput(inputId = ns("selectModelsPred"), label = labelInput("selectMod"),
                                                    choices =  list("knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr"), width = "100%"))
                                ), hr(style = "border-top: 2px solid #cccccc;" ),
                              uiOutput(ns('opcModelsPredN')),

                              actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), width  = "100%" ),br(),br(),
                              withLoader(verbatimTextOutput(ns("txtPredNuevos")),
                                         type = "html", loader = "loader4"))

                 )),
                 col_1(actionButton(ns("modelnext"), label = NULL, width = "100%",
                                    icon = icon("forward")))
        )
        ),
    div(id = ns("cuarta"),
        style = "display:none",
        fluidRow(col_1(actionButton (ns("nuevosback"), label = NULL, width = "100%",
                                     icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModelor",
                     tabPanel(
                       title = p(labelInput("cargarNuev"),class = "wrapper-tag"), width = 12, solidHeader = FALSE,
                       collapsible = FALSE, collapsed = FALSE,value = "CargarNuevos",
                       fluidRow(
                         col_5(checkboxInput(ns('headerNPred2'), labelInput("header"), value = T),
                               checkboxInput(ns('rownameNPred2'), labelInput("Rownames"), value = T),
                               radioButtons(
                                 ns('sepNPred2'), labelInput("separador"), inline = T,
                                 choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')
                               ),
                               radioButtons(ns('decNPred2'), labelInput("separadordec"), c(',', '.'), inline = T),
                               radioSwitch(ns("deleteNAnPred2"), "eliminana", c("eliminar", "imputar")),
                               fileInput(
                                 ns('archivoNPred2'), labelInput("cargarchivo"), width = "100%",
                                 placeholder = "", buttonLabel = labelInput("subir"),
                                 accept = c('text/csv', '.csv', '.txt')), hr(),
                               actionButton(ns("loadButtonNPred2"), labelInput("cargar"), width = "100%"), hr()),
                         col_7(br(), muestra.datos.pred3)),br())
                   )),
                 col_1(actionButton(ns("nuevosnext"), label = NULL, width = "100%",
                                    icon = icon("forward")))
        )
    ),
    div(id = ns("quinta"),
        style = "display:none",
        fluidRow(col_1(actionButton (ns("predicback"), label = NULL, width = "100%",
                                     icon = icon("backward"))),
                 col_10(
                   tabBoxPrmdt(
                     id = "BoxModelo", opciones =  conditionalPanel("input.BoxModelo == 'predicModelo'", tabs.modelos2),
                     tabPanel(title = p(labelInput("predicnuevos"),class = "wrapper-tag"), value = "predicModelo",
                              DT::dataTableOutput(ns("PrediTablePN")),
                              hr(),
                              downloadButton(ns("downloaDatosPred"), labelInput("descargar"), style = "width:100%;"),
                              actionButton(ns("predecirPromidat"), "preditc"),  br())
                   ))
        )
        )
  )
}

#' ind_nuevos Server Function
#'
#' @noRd 
mod_ind_nuevos_server <- function(input, output, session, updateData, newCases){
  ns <- session$ns
  observeEvent(updateData$idioma, {
    
    nombres <- list( "knn", "dt", "rf", "ada", "svm","bayes", "xgb", "nn", "rl", "rlr")
    names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "nn", "rl", "rlr"),updateData$idioma)
    
    updateSelectInput(session, "selectModelsPred", choices = nombres, selected = input$selectModelsPred)
  })
 
  # Load Button Function
  observeEvent(input$loadButtonNPred, {
    rowname    <- isolate(input$rownameNPred)
    ruta       <- isolate(input$archivoNPred)
    sep        <- isolate(input$sepNPred)
    dec        <- isolate(input$decNPred)
    encabezado <- isolate(input$headerNPred)
    deleteNA   <- isolate(input$deleteNAnPred)
    newCases$originales        <- NULL
    newCases$variable.predecir <- NULL
    newCases$datos.aprendizaje <- NULL
    newCases$variable.predecir <- NULL
    newCases$modelo            <- NULL
    newCases$m.seleccionado    <- NULL
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      newCases$originales <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      
      if(ncol(newCases$originales) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        borrar.datos(newCases)
        
      } else {
        newCases$datos.aprendizaje <- newCases$originales
        tabla.trans()
      }
    }, error = function(e) {
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
  })
  
  # Load Button Function (New Cases)
  observeEvent(input$loadButtonNPred2, {
    rowname    <- isolate(input$rownameNPred2)
    ruta       <- isolate(input$archivoNPred2)
    sep        <- isolate(input$sepNPred2)
    dec        <- isolate(input$decNPred2)
    encabezado <- isolate(input$headerNPred2)
    deleteNA   <- isolate(input$deleteNAnPred2)
    variable   <- newCases$variable.predecir
    originales <- newCases$originales
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    
    if(!is.null(variable)){
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      
      test                  <- carga.datos.np(rowname, 
                                              ruta$datapath, 
                                              sep, 
                                              dec, 
                                              encabezado)
      
      #Verifica que los datos contengan las mismas columnas
      if(any(!(c(colnames(test),variable) %in% colnames(originales))))
        stop(tr("NoTamColum", updateData$idioma))
      
      test[,variable]       <- NULL
      test                  <- accion.NAs(test, deleteNA)
      test[,variable]       <- NA
      newCases$datos.prueba <- test
      newCases$datos.prueba[,variable] <- NA
      
      validar()
      unificar.factores()
      
      if(ncol(test) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        newCases$datos.prueba      <- NULL
        newCases$prediccion        <- NULL
        
      } 
    }, error = function(e) {
      newCases$datos.prueba      <- NULL
      newCases$prediccion        <- NULL
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
  }
    else {
      showNotification(
        paste0("Error :", tr("ErrorModelo", updateData$idioma)), duration = 10, type = "error")
      newCases$datos.prueba      <- NULL
      newCases$prediccion        <- NULL
      
    }
  })
  
  #Tabla de datos de aprendizaje
  output$contentsPred <- DT::renderDataTable({
    
    datos  <- newCases$datos.aprendizaje
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )

    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)  
  
  #Tabla de transformar datos
  tabla.trans <- function(){
    output$contentsPred2 <- DT::renderDataTable({
      datos  <- newCases$datos.aprendizaje
      tipos  <- c(
        tr("numerico",   isolate(updateData$idioma)),
        tr("categorico", isolate(updateData$idioma))
      )
      
      tryCatch({
        nombre.columnas <- c("ID", colnames(datos))
        tipo.columnas   <- sapply(colnames(datos), function(i)
          ifelse(class(datos[,i]) %in% c("numeric", "integer"),
                 paste0("<span data-id='numerico'>", tipos[1], "</span>"),
                 paste0("<span data-id='categorico'>", tipos[2], "</span>")))
        sketch = htmltools::withTags(table(
          tableHeader(nombre.columnas),
          tags$tfoot(
            tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
              tags$th(shiny::HTML(i))))
          )
        ))
        DT::datatable(
          datos, selection = 'none', editable = TRUE,  container = sketch,
          options = list(dom = 'frtip', scrollY = "40vh")
        )
      }, error = function(e) {
        showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
        return(NULL)
      })
    }, server = T)
    
  }
  
  #Tabla de datos de prueba
  output$contentsPred3 <- DT::renderDataTable({
    datos  <<- newCases$datos.prueba
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas   <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  # Update Transform Table
  output$transDataPredN = renderUI({
    datos  <- newCases$originales
    idioma <- updateData$idioma
    
    res <- list(fluidRow(
      column(4, tags$span(tags$b("Variable"))),
      column(5, tags$b(tr("tipo", idioma))),
      column(3, tags$b(tr("activa", idioma))),
    ), hr(style = paste0("margin-top: 10px; margin-bottom: 10px;", 
                         "border-top: 1px solid black;")))
    
    if(!is.null(datos) && ncol(datos) > 0) {
      res <- list(res, lapply(colnames(datos), function(x) {
        list(fluidRow(
          column(4, tags$span(x)),
          column(5, selectInputTrans(datos, x, idioma)),
          column(3, tags$input(type = "checkbox", id = ns(paste0("del", x)), 
                               checked = T))
        ), hr(style = "margin-top: 10px; margin-bottom: 10px"))
      }))
    }
    
    res <- tags$div(
      style = "height: 40vh; overflow-y: scroll;",
      do.call(tagList, res)
    )
    return(res)
  })

  
  # Transform Button Function
  observeEvent(input$transButton, {
    datos <- newCases$originales
    cod = ""
    borrar.datos(newCases,  prueba = TRUE)
    newCases$variable.predecir <- NULL
    newCases$modelo            <- NULL
    newCases$m.seleccionado    <- NULL
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    
    for (var in colnames(datos)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    newCases$datos.aprendizaje  <- datos
  }) 
  
  #Crea las opciones de transformar para cada variable
  selectInputTrans <- function(datos, var, idioma = "es") {
    tags$select(
      id = ns(paste0("sel", var)),
      tags$option(value = "categorico", tr("categorico", idioma)),
      if(class(datos[, var]) %in% c("numeric", "integer")) {
        tags$option(value = "numerico", tr("numerico", idioma), 
                    selected = 'selected')
      } else {
        tags$option(value = "numerico", tr("numerico", idioma))
      },
      tags$option(value = "disyuntivo", tr("disyuntivo", idioma))
    )
  }
  
  #Valida que los datos contengan la misma cantidad de columnas 
  validar <- function() {
    cod        <- ""
    originales <-  newCases$originales
    datos      <-  newCases$datos.prueba
    
    tryCatch(
      
    for (var in colnames(originales)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    )
    newCases$datos.prueba <- datos
  }
  
  #Actualiza la cantidad de capas ocultas (neuralnet)
  observeEvent(input$cant.capas.nn.pred, {
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  })

  #Actualiza el texto del modelo
  output$txtPredNuevos <- renderPrint({
    input$PredNuevosBttnModelo
    train                      <- newCases$datos.aprendizaje
    variable                   <- isolate(input$sel.predic.var.nuevos)
    m.seleccionado             <- isolate(input$selectModelsPred)
    newCases$variable.predecir <- NULL
    newCases$modelo            <- NULL
    newCases$m.seleccionado    <- NULL
    newCases$datos.prueba      <- NULL
    newCases$prediccion        <- NULL
    codigo                     <- ""
    cont                       <- 1
    if(m.seleccionado == "rl")
      if(length(levels(train[,variable])) != 2)
        stop(tr("limitModel", updateData$idioma), call. = FALSE)
    
    tryCatch({
      var    <- paste0(variable, "~.")
      codigo <- switch (m.seleccionado ,
                        knn   = {
                          k.value<- isolate(input$kmax.knn.pred)
                          scales <- isolate(input$switch.scale.knn.pred)
                          kernel <- isolate(input$kernel.knn.pred)
                          isolate(modelo <- traineR::train.knn(as.formula(var), data = train, scale = as.logical(scales), kernel = kernel, kmax = k.value ))
                          updateAceEditor(session, "fieldPredNuevos", value = kkn.modelo.np(variable.pr = variable,
                                                                                            scale = scales,
                                                                                            kmax = k.value,
                                                                                            kernel = kernel))
                          isolate(modelo)
                        },
                        dt    = {
                          tipo    <-isolate(input$split.dt.pred)
                          minsplit<-isolate(input$minsplit.dt.pred)
                          maxdepth<-isolate(input$maxdepth.dt.pred)
                          isolate(modelo  <- traineR::train.rpart(as.formula(var), data = train,
                                                         control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),parms = list(split = tipo)))
                          updateAceEditor(session, "fieldPredNuevos", value = dt.modelo.np(variable.pr = variable,
                                                                                           minsplit = minsplit,
                                                                                           maxdepth = maxdepth,
                                                                                           split = tipo))
                          isolate(modelo)
                        },
                        rf    = {
                          mtry   <- isolate(input$mtry.rf.pred)
                          ntree  <- isolate(input$ntree.rf.pred)
                          isolate(modelo <- traineR::train.randomForest(as.formula(var), data = train, mtry = mtry, ntree = ntree, importance = TRUE))
                          updateAceEditor(session, "fieldPredNuevos", value = rf.modelo.np(variable.pr = variable,
                                                                                           ntree = ntree,
                                                                                           mtry  = mtry))
                          isolate(modelo)
                        },
                        svm   = {
                          scales <- isolate(input$switch.scale.svm.pred)
                          k      <- isolate(input$kernel.svm.pred)
                          isolate(modelo <- traineR::train.svm(as.formula(var), data = train, scale = as.logical(scales), kernel = k))
                          updateAceEditor(session, "fieldPredNuevos", value = svm.modelo.np(variable.pr =variable,
                                                                                            scale  = scales,
                                                                                            kernel = k))
                          isolate(modelo)
                        },
                        bayes = {
                          isolate(modelo <- traineR::train.bayes(as.formula(var), data = train))
                          updateAceEditor(session, "fieldPredNuevos", value = bayes.modelo.np(variable.pr=variable))
                          isolate(modelo)
                          
                        },
                        xgb   = {
                          tipo     <- isolate(input$boosterXgb.pred)
                          max.depth<- isolate(input$maxdepthXgb.pred)
                          n.rounds <- isolate(input$nroundsXgb.pred)
                          isolate(modelo   <- traineR::train.xgboost(as.formula(var), data = train, booster = tipo, 
                                                           max_depth = max.depth, nrounds = n.rounds))
                          updateAceEditor(session, "fieldPredNuevos", value = xgb.modelo.np(variable.pr=variable,
                                                                                            booster   = tipo,
                                                                                            max.depth = max.depth,
                                                                                            n.rounds  = n.rounds))
                          isolate(modelo)
                        },
                        rl    = {
                          isolate(modelo <- traineR::train.glm(as.formula(var), data = train))
                          updateAceEditor(session, "fieldPredNuevos", value = rl.modelo.np(variable.pr=variable))
                          isolate(modelo)
                          
                          },
                        nn    = {
                          threshold  <- isolate(input$threshold.nn.pred)
                          stepmax    <- isolate(input$stepmax.nn.pred)
                          capas.np   <- c(isolate(input$nn.cap.pred.1),isolate(input$nn.cap.pred.2),
                                          isolate(input$nn.cap.pred.3),isolate(input$nn.cap.pred.4),
                                          isolate(input$nn.cap.pred.5),isolate(input$nn.cap.pred.6),
                                          isolate(input$nn.cap.pred.7),isolate(input$nn.cap.pred.8),
                                          isolate(input$nn.cap.pred.9),isolate(input$nn.cap.pred.10))
                          cant.capas <- isolate(input$cant.capas.nn.pred)
                          capas.np   <<- as.vector(as.numeric(capas.np[1:cant.capas]))
                          
                          isolate(modelo     <- traineR::train.neuralnet(
                            formula   = as.formula(var),
                            data      = train,
                            threshold = threshold,
                            stepmax   = stepmax,
                            hidden    = capas.np))
                          updateAceEditor(session, "fieldPredNuevos", value = nn.modelo.np(variable.pr=variable,
                                                                                           threshold,
                                                                                           stepmax,
                                                                                           cant.capas,
                                                                                           isolate(input$nn.cap.pred.1),isolate(input$nn.cap.pred.2),
                                                                                           isolate(input$nn.cap.pred.3),isolate(input$nn.cap.pred.4),
                                                                                           isolate(input$nn.cap.pred.5),isolate(input$nn.cap.pred.6),
                                                                                           isolate(input$nn.cap.pred.7),isolate(input$nn.cap.pred.8),
                                                                                           isolate(input$nn.cap.pred.9),isolate(input$nn.cap.pred.10)))
                          isolate(modelo)
                        },
                        rlr    = {
                          scales <- isolate(input$switch.scale.rlr.pred)
                          alpha  <- isolate(input$alpha.rlr.pred)
                          isolate(modelo <- traineR::train.glmnet(as.formula(var), data = train, standardize = as.logical(scales), alpha = alpha, family = 'multinomial' ))
                          updateAceEditor(session, "fieldPredNuevos", value = rlr.modelo.np(variable.pr = variable,
                                                                                            alpha,
                                                                                            scales))
                          isolate(modelo)
                        },
                        ada    = {
                          iter   <- isolate(input$iter.boosting.pred)
                          maxdepth<-isolate(input$maxdepth.boosting.pred)
                          minsplit<-isolate(input$minsplit.boosting.pred)
                          isolate(modelo <- traineR::train.adabag(as.formula(var), data = train, mfinal = iter,
                                                          control = rpart.control(minsplit =minsplit, maxdepth = maxdepth)))
                          updateAceEditor(session, "fieldPredNuevos", value = boosting.modelo.np(variable.pr = variable,
                                                                                                 iter        = iter,
                                                                                                 maxdepth    = maxdepth,
                                                                                                 minsplit    = minsplit))
                          isolate(modelo)
                        }
      )
      newCases$variable.predecir <- variable
      newCases$m.seleccionado    <- m.seleccionado
      newCases$modelo      <- codigo
      print(codigo)
      
    }, error = function(e) {
      if(cont !=1)
      showNotification(paste0("ERROR al generar el modelo: ", e), type = "error")
      cont <- cont + 1
      return(invisible(""))
    },
    warning = function(w){
      if(m.seleccionado == "nn"){
        showNotification(paste0(tr("nnWar", updateData$idioma)," (NN-01) : ",w), duration = 20, type = "warning")
        return(invisible(""))
        
      }        
      if(m.seleccionado == "rl"){
        isolate(modelo <- traineR::train.glm(as.formula(var), data = train))
        updateAceEditor(session, "fieldPredNuevos", value = rl.modelo.np(variable.pr=variable))
        newCases$modelo      <- modelo
        print(modelo)
      }
    })
  })
  
  #Download Prediction Result
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$archivoNPred2$name
    },
    content = function(file) {
      if(!is.null(newCases$prediccion$prediction)){
        write.csv(crear.datos.np(), file, row.names = input$rownameNPred2)
      }
    }
  )
  
  #Genera la tabla de predicciones
  prediccion <- function(){
  output$PrediTablePN <- DT::renderDataTable({
    input$predecirPromidat
    test <- newCases$datos.prueba
    train<- newCases$datos.aprendizaje
    model<- newCases$modelo
    sel  <- newCases$m.seleccionado
    vari <- newCases$variable.predecir
    newCases$prediccion        <- NULL
    tipos  <- c(
        tr("numerico",   isolate(updateData$idioma)),
        tr("categorico", isolate(updateData$idioma))
    )
      tryCatch({
        if(sel == "svm")
        pred                <- predict(model, test[,-which(colnames(test) == vari)], type = 'class')       
        else
        pred                <- predict(model, test, type = 'class')
        
        datos               <- test
        datos[,vari]        <- pred$prediction
        newCases$prediccion <- pred
        updateAceEditor(session, "fieldCodePredPN", value = "predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'class')")
        nombre.columnas <- c("ID", colnames(datos))
        tipo.columnas <- sapply(colnames(datos), function(i)
          ifelse(class(datos[,i]) %in% c("numeric", "integer"),
                 paste0("<span data-id='numerico'>", tipos[1], "</span>"),
                 paste0("<span data-id='categorico'>", tipos[2], "</span>")))
        sketch = htmltools::withTags(table(
          tableHeader(nombre.columnas),
          tags$tfoot(
            tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
              tags$th(shiny::HTML(i))))
          )
        ))
        DT::datatable(
          datos, selection = 'none', editable = TRUE,  container = sketch,
          options = list(dom = 'frtip', scrollY = "40vh")
        )
      }, error = function(e) {
        showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
        return(NULL)
      })
    }, server = T)}



  #Agrega la predicción a los datos
  crear.datos.np <- function(){
    datos.aux.prueba <- newCases$datos.prueba
    datos.aux.prueba[,newCases$variable.predecir]   <- newCases$prediccion$prediction
    
    return(datos.aux.prueba)
  }
  
  #Unifica las variables de tipo factor en training-testing
  unificar.factores <- function(){
    prueba      <- newCases$datos.prueba
    aprendizaje <- newCases$datos.aprendizaje
    for(nombre in colnames(prueba)){
      if(class(prueba[,nombre]) == "factor"){
        levels(prueba[,nombre]) <- unique(c(levels(prueba[,nombre]),
                                                             levels(aprendizaje[,nombre])))
      }
    }
    newCases$datos.prueba <- prueba
  }
  
  # Habilita o deshabilita la semilla RLR
  observeEvent(input$permitir.lambda.pred, {
    if (input$permitir.lambda.pred) {
      shinyjs::enable("lambda.pred")
    } else {
      shinyjs::disable("lambda.pred")
    }
  })
  
  # Wizard Opts Ind.Nuevos--------------------------------------------------------------------------------------------------
  observeEvent(newCases$datos.aprendizaje, {
    if(!is.null(newCases$datos.aprendizaje)){
      shinyjs::show("cargarnext", anim = TRUE, animType = "slide" )
    }
    else{
      shinyjs::hide("cargarnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  
  observeEvent(newCases$datos.prueba, {
    if(!is.null(newCases$datos.prueba)){
      shinyjs::show("nuevosnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("nuevosnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  observeEvent(newCases$modelo, {
    if(!is.null(newCases$modelo)){
      shinyjs::show("modelnext", anim = TRUE, animType = "slide")
    }
    else{
      shinyjs::hide("modelnext", anim = TRUE, animType = "fade")
    }
  },ignoreNULL = FALSE)
  
  observeEvent(input$cargarnext, {
    tabla.trans()
    shinyjs::hide("primera", anim = TRUE )
    shinyjs::show("segundo", anim = TRUE)
  })
  
  observeEvent(input$transback, {
    shinyjs::show("primera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
  })
  
  observeEvent(input$transnext, {
    shinyjs::show("tercera", anim = TRUE)
    shinyjs::hide("segundo", anim = TRUE)
  })
  
  observeEvent(input$modelback, {
    shinyjs::show("segundo", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  
  observeEvent(input$modelnext, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("tercera", anim = TRUE)
  })
  
  observeEvent(input$nuevosback, {
    shinyjs::hide("cuarta",  anim = TRUE)
    shinyjs::show("tercera", anim = TRUE)
  })
  
  observeEvent(input$nuevosnext, {
    prediccion()
    shinyjs::hide("cuarta", anim = TRUE)
    shinyjs::show("quinta", anim = TRUE)
  })  
  
  observeEvent(input$predicback, {
    shinyjs::show("cuarta", anim = TRUE)
    shinyjs::hide("quinta", anim = TRUE)
  })
  # Update Models Options
  output$opcModelsPredN = renderUI({
    datos   <- newCases$datos.aprendizaje
    idioma  <- updateData$idioma
    modelo  <- input$selectModelsPred 
    
    opc_knn <- list(fluidRow(col_4(numericInput(ns("kmax.knn.pred"), tr("kmax", idioma), min = 1,step = 1, value = 7)),
                             col_4(selectInput(inputId = ns("kernel.knn.pred"), label = tr("selkernel", idioma),selected = 1,
                                           choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                       "triweight", "cos","inv","gaussian"))),
                             col_4(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no"),idioma = idioma ))))
    
    opc_svm <- list(fluidRow(col_6(
                                  radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"),idioma = idioma )),
                             col_6(selectInput(inputId = ns("kernel.svm.pred"), label = tr("selkernel", idioma),selected = "radial",
                                          choices = c("linear", "polynomial", "radial", "sigmoid")))))
    
    opc_rf  <- list(fluidRow(col_6(numericInput(ns("ntree.rf.pred"), tr("numTree", idioma), 20, width = "100%", min = 0)),
                             col_6(numericInput(ns("mtry.rf.pred"),  tr("numVars", idioma),1, width = "100%", min = 1))))
    
    opc_dt  <- list(fluidRow(col_4(numericInput(ns("minsplit.dt.pred"), tr("minsplit", idioma), 20, width = "100%",min = 1)),
                             col_4(numericInput(ns("maxdepth.dt.pred"), tr("maxdepth", idioma), 15, width = "100%",min = 0, max = 30, step = 1)),
                             col_4(selectInput(inputId = ns("split.dt.pred"), label = tr("splitIndex", idioma),selected = 1,
                                         choices =  list("gini" = "gini", "Entropia" = "information")))))
    opc_bayes <- list(tags$span())
    
    opc_potenciacion <- list(fluidRow(col_4(numericInput(ns("iter.boosting.pred"), tr("numTree", idioma), 20, width = "100%",min = 1)),
                                      col_4(numericInput(ns("maxdepth.boosting.pred"),tr("maxdepth", idioma), 15, width = "100%",min = 1)),
                                      col_4(numericInput(ns("minsplit.boosting.pred"),tr("minsplit", idioma), 20, width = "100%",min = 1))))
    opc_rl  <- list(tags$span())
   
    opc_rlr <- list(fluidRow(col_6(selectInput(inputId = ns("alpha.rlr.pred"), label = tr("selectAlg", idioma),selected = 1,
                                  choices = list("Ridge" = 0, "Lasso" = 1))),
                             col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"),idioma = idioma )))
                    # ,
                    # fluidRow(col_6(id = ns("colManualLanda"),br(),
                    #                numericInput(ns("lambda.pred"), tr("landa", idioma),value = 2, min = 0, "NULL", width = "100%")), br(),
                    #          col_6(radioSwitchNP(ns("permitir.lambda.pred"), "", c("manual", "automatico"),idioma = idioma )))
                    )
  
    opc_xgb <- list(fluidRow(col_4(selectInput(inputId = ns("boosterXgb.pred"), label = tr("selbooster", idioma), selected = 1,
                                               choices = c("gbtree", "gblinear", "dart"))),
                             col_4(numericInput(ns("maxdepthXgb.pred"), tr("maxdepth", idioma),  min = 1,  step = 1, value = 6)),
                             col_4(numericInput(ns("nroundsXgb.pred"),  tr("selnrounds", idioma), min = 0, step = 1, value = 50))))
  
    opc_nn <- list(fluidRow(col_4(numericInput(ns("threshold.nn.pred"),tr("threshold", idioma),
                                               min = 0,   step = 0.01, value = 0.05)),
                            col_4(numericInput(ns("stepmax.nn.pred"),tr("stepmax", idioma),
                                               min = 100, step = 100,  value = 5000)),
                            col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                              label = tr("selectCapas", idioma), value = 10))),
                   fluidRow(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                            col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 2),
                                               class = "mini-numeric-select")))))

    res <-  switch(modelo,
                   knn   =  opc_knn,
                   svm   =  opc_svm,
                   rf    =  opc_rf,
                   bayes =  opc_bayes,
                   nn    =  opc_nn,
                   ada   =  opc_potenciacion,
                   xgb   =  opc_xgb,
                   rl    =  opc_rl,
                   rlr   =  opc_rlr,
                   dt    =  opc_dt)
    
    if(!is.null(newCases$datos.aprendizaje)){
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.categoricas(newCases$datos.aprendizaje))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(newCases$datos.aprendizaje))))
      updateNumericInput(session, "mtry.rf.pred",  value = round(sqrt(ncol(newCases$datos.aprendizaje) -1)))
      
    }
  
    res <-  do.call(tagList, res)

    return(res)
  })
}
    
## To be copied in the UI
# mod_ind_nuevos_ui("ind_nuevos_ui_1")
    
## To be copied in the server
# callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1", updateData)
 
