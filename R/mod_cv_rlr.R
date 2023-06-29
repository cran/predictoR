#' cv_rlr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rlr_ui <- function(id){
  ns <- NS(id)
  

  tagList(
    tabBoxPrmdt(
      id = ns("Boxrlr"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrlrModelo",
               div(col_6(radioSwitch(ns("scale_cvrlr"), "escal", c("si", "no"))),
               col_6(
                 selectizeInput(
                   ns("sel_alpha"), labelInput("selectAlg"), multiple = T,
                   choices = list("Ridge" = 0, "Lasso" = 1)))),
               
               fluidRow(col_6(numericInput(ns("cvrlr_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvrlr_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrlr")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rlr"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrlrIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_rlr_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_rlr_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVrlrIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_rlr_category"), width = "100%", height = "70vh")), 
                   col_6(echarts4rOutput(ns("e_rlr_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_rlr Server Functions
#'
#' @noRd 
mod_cv_rlr_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.rlr = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.rlr <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_alpha", selected = "")
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvrlr_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvrlr_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvrlr_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvrlr_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvrlr_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvrlr <- renderPrint({
      input$btn_cv_rlr
      M$MCs.rlr <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        alphas       <- isolate(input$sel_alpha)# Algoritmos seleccionados para CV (vector)
        alpha_labels <- alphas # Algoritmos seleccionados para CV (vector)
        cant.vc   <- isolate(updateData$numValC)# Obtiene cantidad de validaciones a realizar
        MCs.rlr   <- vector(mode = "list")# Lista de listas que va a guardar todas las MCs
        datos     <- isolate(updateData$datos)# Obtiene los datos
        numGrupos <- isolate(updateData$numGrupos)# Obtiene la cantidad de grupos
        grupos    <- isolate(updateData$grupos)# Obtiene los grupos de cada validación
        scales    <- isolate(input$scale_cvrlr)
        variable  <- updateData$variable.predecir# Variable a predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))# Categorías de la variable a predecir
        dim_v     <- isolate(length(category))# Cantidad de categorías (para generar las matrices de confusión)
        nombres   <- vector(mode = "character", length = length(alphas))# Almacena el nombre de los modelos (vector en caso de varios kernels, uno solo en caso que no aplican los kernels)
        Corte     <- isolate(input$cvrlr_step)# Obtiene la probabilidad de corte para el modelo
        cat_sel   <- isolate(input$cvrlr_cat)# Obtiene la categoría de la variable a predecir seleccionada para aplicar probabilidad de corte
        
        # Modifica las etiquetas de los algoritmos para que no se muestre 0 y 1 sino Ridge y Lasso
        alpha_labels[which(alpha_labels == 0)] = "Ridge"
        alpha_labels[which(alpha_labels == 1)] = "Lasso"
        
        if(length(alphas)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un alpha")
        }
        for (alpha in 1:length(alphas)){
          # Llena la lista de listas de MCs con los nombres de cada modelo
          MCs.rlr[[paste0("MCs.",alpha_labels[alpha])]] <- vector(mode = "list", length = cant.vc)
          # Guarda los nombres para las matrices individuales
          nombres[alpha] <- paste0("MC.",alpha_labels[alpha])
        }
        
        for (i in 1:cant.vc){
          # Lista de Matrices, se identifican con el nombre del modelo
          MC.rlr <- vector(mode = "list", length = length(alphas))
          names(MC.rlr) <- nombres
          # Crea la matriz que almacena la MC de confusión
          # Toma en cuenta las dimensiones de la variable a predecir con dim_v
          for (alpha in 1:length(alphas)){
            MC.rlr[[alpha]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            # Obtiene los grupos de cada validación
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(alphas)){
              # Genera el modelo
              modelo <- traineR::train.glmnet(as.formula(var_), 
                                              data        = ttraining, 
                                              standardize = as.logical(scales), 
                                              alpha       = alphas[j], 
                                              family      = 'multinomial' )
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
                Score       <- prediccion$prediction[,positive,]
                # Genera la predicción con el corte y categoría seleccionada
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                # Crea la MC
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                # Suma la MC
                MC.rlr[[j]] <- MC.rlr[[j]] + MC
              }else{
                # Para el caso de 3 o más categorías
                # Predicción, MC 
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.rlr[[j]] <- MC.rlr[[j]] + MC
              }
            }
          }
          
          # Guarda las matrices en la lista de matrices
          for (l in 1:length(MCs.rlr)){
            MCs.rlr[[l]][[i]] <- MC.rlr[[l]]
          }
        }
        
        # Asigna los valores a las variables reactivas
        M$MCs.rlr  <- MCs.rlr
        # Se calculan los indices para realizar los gráficos
        resultados <- indices.cv(category, cant.vc, alpha_labels, MCs.rlr)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_rlr_code(variable, dim_v, cant.vc, numGrupos)))
        
        print(MCs.rlr)
        
      },error = function(e){
        M$MCs.rlr <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    # Gráfico de la precisión Global
    output$e_rlr_glob  <-  renderEcharts4r({
      input$btn_cv_rlr
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), "Alpha" ))), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma), "Alpha", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    # Gráfico de error Global
    output$e_rlr_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), "Alpha" ))), 
                "error"  = return( resumen.error(err,  labels = c(tr("errG",idioma), "Alpha", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    # Gráfico de precisión por categoría
    output$e_rlr_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), "Alpha" ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0(tr("prec",idioma), " ",cat ), "Alpha", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    # Gráfico de error por categoría
    output$e_rlr_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1 - M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), "Alpha" ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0("Error ",cat ), "Alpha", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_rlr_ui("cv_rlr_1")
    
## To be copied in the server
# mod_cv_rlr_server("cv_rlr_1")
