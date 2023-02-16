#' cv_bayes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_bayes_ui <- function(id){
  ns <- NS(id)
  
  
  title_comp <- list(conditionalPanel("input['cv_bayes_ui_1-Boxbayes'] == 'tabCVbayesIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxbayes"),
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVbayesModelo",
               fluidRow(col_6(numericInput(ns("cvbayes_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvbayes_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvbayes")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_bayes"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVbayesIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_bayes_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_bayes_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVbayesIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_bayes_category"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_bayes_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_bayes Server Functions
#'
#' @noRd 
mod_cv_bayes_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.bayes = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir,updateData$numGrupos, updateData$numValC), {
      M$MCs.bayes <- NULL
      M$grafico   <- NULL
      M$global    <- NULL
      M$categories <- NULL
      M$times      <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvbayes_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvbayes_cat", anim = TRUE, animType = "fade")
          shinyjs::show("cvbayes_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvbayes_cat", anim = TRUE, animType = "fade")
          shinyjs::hide("cvbayes_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvbayes <- renderPrint({
      input$btn_cv_bayes
      M$MCs.bayes <- NULL
      M$grafico   <- NULL
      M$global    <- NULL
      M$categories <- NULL
      tryCatch({
        
        cant.vc   <- updateData$numValC
        MCs.bayes <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- updateData$numGrupos
        grupos    <- updateData$grupos
        variable  <- isolate(updateData$variable.predecir)
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombre    <- "MC.bayes"
        Corte     <- isolate(input$cvbayes_step)
        cat_sel   <- isolate(input$cvbayes_cat)

        MCs.bayes[["MCs.bayes"]] <- vector(mode = "list", length = cant.vc)

        for (i in 1:cant.vc){
          MC.bayes <- vector(mode = "list", length = 1)
          names(MC.bayes) <- nombre 
          MC.bayes[[1]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            j <- 1
              modelo      <- train.bayes(as.formula(var_), 
                                         data = ttraining)
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.bayes[[j]] <- MC.bayes[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.bayes[[j]] <- MC.bayes[[j]] + MC
              }
          }
          
          for (l in 1:length(MCs.bayes)){
            MCs.bayes[[l]][[i]] <- MC.bayes[[l]]
          }
        }
        
        M$MCs.bayes <- MCs.bayes
        resultados  <- indices.cv(category, cant.vc, c("bayes"), MCs.bayes)
        M$grafico   <- resultados$grafico
        M$global    <- resultados$global
        M$categories <- resultados$categories
        M$times     <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_bayes_code(variable, dim_v, cant.vc, numGrupos)))
        
        print(MCs.bayes)
        
      },error = function(e){
        M$MCs.bayes <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_bayes_glob  <-  renderEcharts4r({
      input$btn_cv_bayes
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), tr("modelo",idioma) ))), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    output$e_bayes_error  <-  renderEcharts4r({
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
    
    
    output$e_bayes_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("modelo",idioma)))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0(tr("prec",idioma), " ",cat ), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    output$e_bayes_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), tr("modelo",idioma)))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0("Error ",cat ), tr("modelo",idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
}

    
## To be copied in the UI
# mod_cv_bayes_ui("cv_bayes_1")
    
## To be copied in the server
# mod_cv_bayes_server("cv_bayes_1")
