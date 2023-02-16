#' cv_rl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rl_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxrl"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrlModelo",
               fluidRow(col_6(numericInput(ns("cvrl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvrl_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrl")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rl"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrlIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_rl_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_rl_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVrlIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_rl_category"), width = "100%", height = "70vh")), 
                   col_6(echarts4rOutput(ns("e_rl_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_rl Server Functions
#'
#' @noRd 
mod_cv_rl_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.rl = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      M$MCs.rl <- NULL
      M$grafico   <- NULL
      M$global    <- NULL
      M$categories <- NULL
      M$times      <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvrl_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvrl_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvrl_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvrl_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvrl_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvrl <- renderPrint({
      input$btn_cv_rl
      M$MCs.rl <- NULL
      M$grafico   <- NULL
      M$global    <- NULL
      M$categories <- NULL
      tryCatch({
        
        cant.vc   <- updateData$numValC
        MCs.rl    <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- updateData$numGrupos
        grupos    <- updateData$grupos
        variable  <- isolate(updateData$variable.predecir)
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombre    <- "MC.rl"
        Corte     <- isolate(input$cvrl_step)
        cat_sel   <- isolate(input$cvrl_cat)

        MCs.rl[["MCs.rl"]] <- vector(mode = "list", length = cant.vc)

        for (i in 1:cant.vc){
          MC.rl <- vector(mode = "list", length = 1)
          names(MC.rl) <- nombre 
          MC.rl[[1]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            j <- 1
              modelo      <- train.glm(as.formula(var_), 
                                       data = ttraining)
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.rl[[j]] <- MC.rl[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.rl[[j]] <- MC.rl[[j]] + MC
              }
          }
          
          for (l in 1:length(MCs.rl)){
            MCs.rl[[l]][[i]] <- MC.rl[[l]]
          }
        }
        
        M$MCs.rl <- MCs.rl
        resultados  <- indices.cv(category, cant.vc, c("rl"), MCs.rl)
        resultados$grafico$name <- tr(c("rl"),codedioma$idioma)
        M$grafico   <- resultados$grafico
        M$global    <- resultados$global
        M$categories <- resultados$categories
        M$times     <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_rl_code(variable, dim_v, cant.vc, numGrupos)))
        
        print(MCs.rl)
        
      },error = function(e){
        M$MCs.rl <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_rl_glob  <-  renderEcharts4r({
      input$btn_cv_rl
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma),  tr("modelo",idioma) ))), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma),  tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),  tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    output$e_rl_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), tr("modelo",idioma) ))), 
                "error"  = return( resumen.error(err,  labels = c(tr("errG",idioma), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    output$e_rl_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("modelo",idioma) ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0(tr("prec",idioma), " ",cat ), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    output$e_rl_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1 - M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), tr("modelo",idioma) ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0("Error ",cat ), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_rl_ui("cv_rl_1")
    
## To be copied in the server
# mod_cv_rl_server("cv_rl_1")
