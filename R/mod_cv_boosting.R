#' cv_boost UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_boost_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxboost"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVboostModelo",
               div(col_6(numericInput(ns("mfinal"), labelInput("numTree"), 20, width = "100%",min = 1)),
                        col_6(numericInput(ns("maxdepth"), labelInput("maxdepth"), 15, width = "100%",min = 1))),
               div(col_12(
                 selectizeInput(
                   ns("sel_kernel"), labelInput("selkernel"), multiple = T,
                   choices = c("Breiman", "Freund", "Zhu"))
                 )),
               fluidRow(col_6(numericInput(ns("cvboost_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                   col_6(selectInput(ns("cvboost_cat"), choices = "",label =  labelInput("selectCat"), width = "100%"))), 
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvboost")), 
                                                      type = "html", loader = "loader4")),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_boost"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVboostIndices",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("barras", "lineas", "error"), width = "100%")))), hr()),
               div(col_6(echarts4rOutput(ns("e_boost_glob"), width = "100%", height = "70vh")),
                   col_6(echarts4rOutput(ns("e_boost_error"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabCVboostIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_boost_category"), width = "100%", height = "70vh")), 
                   col_6(echarts4rOutput(ns("e_boost_category_err"), width = "100%", height = "70vh"))))
    )
 
  )
}
    
#' cv_boost Server Functions
#'
#' @noRd 
mod_cv_boost_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    
    M <- rv(MCs.boost = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    observeEvent(codedioma$idioma, {
      nombres <- list( "lineas", "barras","error")
      names(nombres) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      
      updateSelectInput(session, "plot_type", choices = nombres, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
    })
    
    observeEvent(c(updateData$datos, updateData$variable.predecir, updateData$grupos), {
      M$MCs.boost <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times      <- 0
      datos        <- updateData$datos
      variable     <- updateData$variable.predecir
      
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable]))
        updateSelectizeInput(session, "sel_kernel", selected = "")
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1])
        updateSelectInput(session, "cvboost_cat", choices = choices, selected = choices[1])
        if(length(choices) == 2){
          shinyjs::show("cvboost_cat",  anim = TRUE, animType = "fade")
          shinyjs::show("cvboost_step", anim = TRUE, animType = "fade")
        }else{
          shinyjs::hide("cvboost_cat",  anim = TRUE, animType = "fade")
          shinyjs::hide("cvboost_step", anim = TRUE, animType = "fade")
        }
      }
      
    })
    
    output$txtcvboost <- renderPrint({
      input$btn_cv_boost
      M$MCs.boost <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      tryCatch({
        kernels   <- isolate(input$sel_kernel)
        cant.vc   <- isolate(updateData$numValC)
        MCs.boost <- vector(mode = "list")
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        mfinal    <- isolate(input$mfinal)
        maxdepth  <-isolate(input$maxdepth)
        #minsplit  <-isolate(input$minsplit)
        variable  <- updateData$variable.predecir
        var_      <- paste0(variable, "~.")
        category  <- isolate(levels(updateData$datos[,variable]))
        dim_v     <- isolate(length(category))
        nombres   <- vector(mode = "character", length = length(kernels))
        Corte     <- isolate(input$cvboost_step)
        cat_sel   <- isolate(input$cvboost_cat)
        
        if(length(kernels)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un kernel")
        }
        for (kernel in 1:length(kernels)){
          MCs.boost[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
          nombres[kernel] <- paste0("MC.",kernels[kernel])
        }
        
        for (i in 1:cant.vc){
          MC.boost <- vector(mode = "list", length = length(kernels))
          names(MC.boost) <- nombres
          for (kernel in 1:length(kernels)){
            MC.boost[[kernel]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
          }
          
          for (k in 1:numGrupos){
            muestra   <- grupos[[i]][[k]]
            ttraining <- datos[-muestra, ]
            ttesting  <- datos[muestra, ]
            
            for (j in 1:length(kernels)){
              modelo      <- train.adabag(as.formula(var_), 
                                          data      = ttraining, 
                                          coeflearn = kernels[j], 
                                          mfinal    = mfinal,
                                          control = rpart.control(maxdepth = maxdepth))
              if(length(category) == 2){
                positive    <- category[which(category == cat_sel)]
                negative    <- category[which(category != cat_sel)]
                prediccion  <- predict(modelo, ttesting, type = "prob")
                Clase       <- ttesting[,variable]
                Score       <- prediccion$prediction[,positive]
                Prediccion  <- ifelse(Score  > Corte, positive, negative)
                MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                MC.boost[[j]] <- MC.boost[[j]] + MC
              }else{
                prediccion  <- predict(modelo, ttesting)
                MC          <- confusion.matrix(ttesting, prediccion)
                MC.boost[[j]] <- MC.boost[[j]] + MC
              }
            }
          }
          
          for (l in 1:length(MCs.boost)){
            MCs.boost[[l]][[i]] <- MC.boost[[l]]
          }
        }
        
        M$MCs.boost  <- MCs.boost
        resultados <- indices.cv(category, cant.vc, kernels, MCs.boost)
        M$grafico  <- resultados$grafico
        M$global   <- resultados$global
        M$categories <- resultados$categories
        M$times    <- 1
        isolate(codedioma$code <- append(codedioma$code, cv_boost_code(variable, dim_v, cant.vc, numGrupos)))
        
        print(MCs.boost)
        
      },error = function(e){
        M$MCs.boost <- NULL
        M$grafico <- NULL
        M$global  <- NULL
        M$categories <- NULL
        M$times    <- 0
        return(invisible(""))
      })
    })
    
    
    
    output$e_boost_glob  <-  renderEcharts4r({
      input$btn_cv_boost
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), "Kernel" ))), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma), "Kernel", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    output$e_boost_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p
      
      if(!is.null(M$grafico)){
        err  <- M$grafico
        err$value <- 1 - M$global
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), "Kernel" ))), 
                "error"  = return( resumen.error(err,  labels = c(tr("errG",idioma), "Kernel", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    output$e_boost_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), "Kernel" ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0(tr("prec",idioma), " ",cat ), "Kernel", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    output$e_boost_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      cat    <- input$cv.cat.sel
      type   <- input$plot_type
      if(!is.null(M$grafico)){
        graf  <- M$grafico
        graf$value <- 1- M$categories[[cat]]
        switch (type,
                "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat ), "Kernel" ))), 
                "error"  = return( resumen.error(graf,  labels = c(paste0("Error ",cat ), "Kernel", tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
}

    
## To be copied in the UI
# mod_cv_boost_ui("cv_boost_1")
    
## To be copied in the server
# mod_cv_boost_server("cv_boost_1")
