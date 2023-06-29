#' varerr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_varerr_ui <- function(id){
  ns <- NS(id)

  tagList(
    tabBoxPrmdt(id = "Boxvarerr",
                tabPanel(title = labelInput("varError"), value = "tabModelosComp", 
                         div(
                           col_8(),
                           col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectMod"),class = "wrapper-tag"),
                                       tags$div(class="multiple-select-var",
                                                selectInput(inputId = ns("model.sel"),label = NULL,
                                                            choices =  "", width = "100%"))))),
                        div(col_6(withLoader(echarts4rOutput(ns('plot_comp'), height = "70vh"), 
                                       type = "html", loader = "loader4")), 
                            col_6(withLoader(echarts4rOutput(ns('plot_comp_err'), height = "70vh"), 
                                              type = "html", loader = "loader4")))
                         ))
  )
}

#' varerr Server Function
#'
#' @noRd 
mod_varerr_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns

  observeEvent(codedioma$idioma, {
    select   <-  ifelse(is.null(input$model.sel), nombres[1], input$model.sel)
    
    nombres <- list("knnl", "svml", "dtl", "rfl", "xgb" , "bl", "Bayes", "rl", "rlr", "lda", "qda")
    names(nombres) <- tr(c("knnl", "svml", "dtl", "rfl", "xgb" , "bl", "Bayes", "rl", "rlr", "lda", "qda"),codedioma$idioma)
    updateSelectInput(session, "model.sel", choices = nombres, selected = select)
  })
  
    # Update Plot ROC
    output$plot_comp <- renderEcharts4r({
      idioma        <- codedioma$idioma
      mdls          <- modelos2
      isolate(category   <- levels(updateData$datos[,updateData$variable.predecir]))
      selected <- input$model.sel
      tryCatch({
        
        mdl <- switch (selected,
                       "knnl"  = mdls$knn,
                       "svml"  = mdls$svm,
                       "dtl"   = mdls$dt,
                       "rfl"   = mdls$rf,
                       "bl"    = mdls$boosting, 
                       "Bayes" = mdls$bayes, 
                       "xgb"   = mdls$xgb, 
                       "rl"    = mdls$rl, 
                       "rlr"   = mdls$rlr, 
                       "lda"   = mdls$lda, 
                       "qda"   = mdls$qda)
        
        if(is.null(mdl$mcs[[1]]))
          return(NULL)
        indices <- indices.comp(category, mdl$mcs,mdl$n )
        graf <- indices$grafico
        comp.lineas(graf, labels = c(tr("precG",idioma), tr("rep",idioma) ))
      }, error = function(e) {
        showNotification(e, duration = 15, type = "error")
        return(NULL)
      })
    })
    
    # Update Plot ROC
    output$plot_comp_err <- renderEcharts4r({
      idioma   <- codedioma$idioma
      mdls     <- modelos2
      selected <- input$model.sel
      isolate(category   <- levels(updateData$datos[,updateData$variable.predecir]))
      tryCatch({
        
        mdl <- switch (selected,
                       "knnl"  = mdls$knn,
                       "svml"  = mdls$svm,
                       "dtl"   = mdls$dt,
                       "rfl"   = mdls$rf,
                       "bl"    = mdls$boosting, 
                       "Bayes" = mdls$bayes, 
                       "xgb"   = mdls$xgb, 
                       "rl"    = mdls$rl, 
                       "rlr"   = mdls$rlr, 
                       "lda"   = mdls$lda, 
                       "qda"   = mdls$qda)
        
        if(is.null(mdl$mcs[[1]]))
          return(NULL)
        indices <- indices.comp(category, mdl$mcs,mdl$n )
        graf    <- indices$grafico
        graf$value <- 1 - graf$value
        comp.lineas(graf, labels = c(tr("errG",idioma), tr("rep",idioma) ))
      }, error = function(e) {
        showNotification(e, duration = 15, type = "error")
        return(NULL)
      })
    })
}

## To be copied in the UI
# mod_varerr_ui("varerr_ui_1")
    
## To be copied in the server
# callModule(mod_varerr_server, "varerr_ui_1")
 
