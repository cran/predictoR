#' cv_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_dt_ui <- function(id){
  ns <- NS(id)

  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxdt"),
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVdtModelo",
               div(col_6(numericInput(ns("max_depth"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                        col_6(numericInput(ns("min_split"), labelInput("minsplit"),min = 1,step = 1, value = 2))),
               div(col_12(
                 selectizeInput(
                   ns("sel_split"), labelInput("splitIndex"), multiple = T,
                   choices =  list("gini" = "gini", "Entropia" = "information")))),
               
               fluidRow(col_6(numericInput(ns("cvdt_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvdt_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvdt")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_dt"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVdtIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_dt_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_dt_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVdtIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_dt_category"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_dt_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_dt Server Functions
#'
#' @noRd 
mod_cv_dt_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.dt = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.dt  <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times      <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_split", selected = "")
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvdt_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvdt_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvdt_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvdt_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvdt_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvdt <- renderPrint({
      input$btn_cv_dt
      M$MCs.dt <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        splits    <- isolate(input$sel_split) # Algoritmos seleccionados para CV (vector)
        cant.vc   <- isolate(updateData$numValC)# Obtiene cantidad de validaciones a realizar
        MCs.dt    <- vector(mode = "list")# Lista de listas que va a guardar todas las MCs
        datos     <- isolate(updateData$datos)# Obtiene los datos
        numGrupos <- isolate(updateData$numGrupos)# Obtiene la cantidad de grupos
        grupos    <- isolate(updateData$grupos)# Obtiene los grupos de cada validación
        max_depth <- isolate(input$max_depth)# Profundidad máxima de los árboles
        min_split <- isolate(input$min_split)# Mínimo para dividir un nodo
        variable  <- updateData$variable.predecir# Variable a predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))# Categorías de la variable a predecir
        dim_v     <- isolate(length(category))# Cantidad de categorías (para generar las matrices de confusión)
        nombres   <- vector(mode = "character", length = length(splits))# Almacena el nombre de los modelos (vector en caso de varios kernels, uno solo en caso que no aplican los kernels)
        Corte     <- isolate(input$cvdt_step)# Obtiene la probabilidad de corte para el modelo
        cat_sel   <- isolate(input$cvdt_cat)# Obtiene la categoría de la variable a predecir seleccionada para aplicar probabilidad de corte
        
        if(length(splits)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un kernel")
        }
        for (kernel in 1:length(splits)){
          # Llena la lista de listas de MCs con los nombres de cada modelo
          MCs.dt[[paste0("MCs.",splits[kernel])]] <- vector(mode = "list", length = cant.vc)
          # Guarda los nombres para las matrices individuales
          nombres[kernel] <- paste0("MC.",splits[kernel])
        }
        
        for (i in 1:cant.vc){
          # Lista de Matrices, se identifican con el nombre del modelo
          MC.dt <- vector(mode = "list", length = length(splits))
          names(MC.dt) <- nombres
          # Crea la matriz que almacena la MC de confusión
          # Toma en cuenta las dimensiones de la variable a predecir con dim_v
          for (kernel in 1:length(splits)){
            MC.dt[[kernel]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            # Obtiene los grupos de cada validación
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            # Recorre los algoritmos seleccionados
            for (j in 1:length(splits)){
              # Genera el modelo
              modelo      <- train.rpart(as.formula(var_), data = ttraining,
                                         control = rpart.control(minsplit = min_split, maxdepth = max_depth),parms = list(split = splits[j]))
              if(length(category) == 2){
                # Se define la categoría positiva y negativa
                # Categoría positiva se asume es la seleccionada 
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                # Genera las probabilidades de predicción
                prediccion  <- predict(modelo, ttesting, type = "prob")
                # Guarda la clase verdadera
                Clase       <- ttesting[,variable]
                # Obtiene las probabilidades para la categoría seleccionada
                Score       <- prediccion$prediction[,positive]
                # Genera la predicción con el corte y categoría seleccionada
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                # Crea la MC
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                # Suma la MC
                MC.dt[[j]]  <- MC.dt[[j]] + MC
              }else{
                # Para el caso de 3 o más categorías
                # Predicción, MC 
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.dt[[j]] <- MC.dt[[j]] + MC
              }
            }
          }
          
          # Guarda las matrices en la lista de matrices
          for (l in 1:length(MCs.dt)){
            MCs.dt[[l]][[i]] <- MC.dt[[l]]
          }
        }
        
        # Asigna los valores a las variables reactivas
        M$MCs.dt  <- MCs.dt
        # Se calculan los indices para realizar los gráficos
        resultados <- indices.cv(category, cant.vc, splits, MCs.dt)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_dt_code(variable, dim_v, cant.vc, numGrupos)))
        
        print(MCs.dt)
        
      },error = function(e){
        M$MCs.dt <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    # Gráfico de la precisión Global
    output$e_dt_glob  <-  renderEcharts4r({
      input$btn_cv_dt
      type    <- input$plot_type_p # Tipo de gráfico seleccionado
      grafico <- M$grafico # Datos del gráfico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), unlist(strsplit(tr("splitIndex",idioma), ":")) ))), 
                "error" = return( resumen.error(grafico, labels = c(tr("precG",idioma), unlist(strsplit(tr("splitIndex",idioma), ":")), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    # Gráfico del error Global
    output$e_dt_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p # Tipo de gráfico seleccionado
      
      if(!is.null(M$grafico)){
        err  <- M$grafico # Datos del gráfico
        err$value <- 1 - M$global
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), unlist(strsplit(tr("splitIndex",idioma), ":")) ))), 
                "error" = return( resumen.error(err, labels = c(tr("errG",idioma), unlist(strsplit(tr("splitIndex",idioma), ":")), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    # Gráfico de precisión por categoría
    output$e_dt_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel # Categoría seleccionada
      type   <- input$plot_type # Tipo de gráfico seleccionado
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), unlist(strsplit(tr("splitIndex",idioma), ":")) ))), 
                "error" = return( resumen.error(graf,   labels = c(paste0(tr("prec",idioma), " ",cat ), unlist(strsplit(tr("splitIndex",idioma), ":")), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    # Gráfico de error por categoría
    output$e_dt_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel # Categoría seleccionada
      type   <- input$plot_type # Tipo de gráfico seleccionado
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), unlist(strsplit(tr("splitIndex",idioma), ":")) ))), 
                "error" = return( resumen.error(graf,   labels = c(paste0("Error ",cat ), unlist(strsplit(tr("splitIndex",idioma), ":")), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_dt_ui("cv_dt_1")
    
## To be copied in the server
# mod_cv_dt_server("cv_dt_1")
