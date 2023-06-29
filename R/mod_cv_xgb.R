#' cv_xgb UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_xgb_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxdt"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabcvxgbModelo",
               div(col_6(numericInput(ns("max_depth"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                        col_6(numericInput(ns("n_rounds"), labelInput("selnrounds"),min = 1,step = 1, value = 50))),
               div(col_12(
                 selectizeInput(
                   ns("sel_booster"), labelInput("selbooster"), multiple = T,
                   choices =  c("gbtree", "gblinear", "dart")))),
               
               fluidRow(col_6(numericInput(ns("cvxgb_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvxgb_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvxgb")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_xgb"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvxgbIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_xgb_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_xgb_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabcvxgbIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_xgb_category"), width = "100%", height = "70vh")), 
                   col_6(echarts4rOutput(ns("e_xgb_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_xgb Server Functions
#'
#' @noRd 
mod_cv_xgb_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.dt = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.dt <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_booster", selected = "")
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvxgb_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvxgb_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvxgb_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvxgb_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvxgb_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvxgb <- renderPrint({
      input$btn_cv_xgb
      M$MCs.dt <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        boosters    <- isolate(input$sel_booster)
        cant.vc   <- isolate(updateData$numValC)# Obtiene cantidad de validaciones a realizar
        MCs.dt    <- vector(mode = "list")# Lista de listas que va a guardar todas las MCs
        datos     <- isolate(updateData$datos)# Obtiene los datos
        numGrupos <- isolate(updateData$numGrupos)# Obtiene la cantidad de grupos
        grupos    <- isolate(updateData$grupos)# Obtiene los grupos de cada validación
        max_depth <- isolate(input$max_depth)
        n_rounds  <- isolate(input$n_rounds)
        variable  <- updateData$variable.predecir# Variable a predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))# Categorías de la variable a predecir
        dim_v     <- isolate(length(category))# Cantidad de categorías (para generar las matrices de confusión)
        nombres   <- vector(mode = "character", length = length(boosters))# Almacena el nombre de los modelos (vector en caso de varios kernels, uno solo en caso que no aplican los kernels)
        Corte     <- isolate(input$cvxgb_step)# Obtiene la probabilidad de corte para el modelo
        cat_sel   <- isolate(input$cvxgb_cat)# Obtiene la categoría de la variable a predecir seleccionada para aplicar probabilidad de corte
        
        if(length(boosters)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un booster")
        }
        for (booster in 1:length(boosters)){
          # Llena la lista de listas de MCs con los nombres de cada modelo
          MCs.dt[[paste0("MCs.",boosters[booster])]] <- vector(mode = "list", length = cant.vc)
          # Guarda los nombres para las matrices individuales
          nombres[booster] <- paste0("MC.",boosters[booster])
        }
        
        for (i in 1:cant.vc){
          MC.dt <- vector(mode = "list", length = length(boosters))
          names(MC.dt) <- nombres
          for (booster in 1:length(boosters)){
            MC.dt[[booster]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(boosters)){
              modelo      <- train.xgboost(as.formula(var_), 
                                           data = ttraining,
                                           max_depth = max_depth, 
                                           booster = boosters[j], 
                                           nrounds = n_rounds)
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.dt[[j]]  <- MC.dt[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.dt[[j]] <- MC.dt[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.dt)){
            MCs.dt[[l]][[i]] <- MC.dt[[l]]
          }
        }
        
        M$MCs.dt  <- MCs.dt
        resultados <- indices.cv(category, cant.vc, boosters, MCs.dt)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_xgb_code(variable, dim_v, cant.vc, numGrupos)))
        
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
    
    
    
    output$e_xgb_glob  <-  renderEcharts4r({
      input$btn_cv_xgb
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), tr("booster",idioma)))), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma), tr("booster",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    output$e_xgb_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), tr("booster",idioma)))), 
                "error"  = return( resumen.error(err,  labels = c(tr("errG",idioma), tr("booster",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    output$e_xgb_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ),tr("booster",idioma) ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0(tr("prec",idioma), " ",cat ), tr("booster",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    output$e_xgb_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1 - M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), tr("booster",idioma) ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0("Error ",cat ), tr("booster",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_xgb_ui("cv_xgb_1")
    
## To be copied in the server
# mod_cv_xgb_server("cv_xgb_1")
