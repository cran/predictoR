#' poder_pred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_poder_pred_ui <- function(id){
  ns <- NS(id)
  opc_podpred <- fluidRow(
    conditionalPanel(
    "input.BoxPodPred == 'tabDistpred' || input.BoxPodPred == 'tabPares'",
    tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
    list(
      conditionalPanel(
        "input.BoxPodPred == 'tabDistpred'",
        codigo.monokai(ns("fieldCodeDistpred"), height = "10vh")),
      conditionalPanel(
        "input.BoxPodPred == 'tabPares'",
        codigo.monokai(ns("fieldCodePares"), height = "10vh")))
  ))),
  conditionalPanel(
    "input.BoxPodPred == 'tabDistpredcat' || input.BoxPodPred == 'tabDenspred'",
    tabsOptions(heights = c(70, 30), tabs.content = list(
      list(options.base(), tags$hr(style = "margin-top: 0px;"),
           conditionalPanel(
             "input.BoxPodPred == 'tabDistpredcat'",
             selectInput(label = labelInput("selvar"), inputId = ns("sel_pred_cat"), choices = "")
           ),
           conditionalPanel(
             "input.BoxPodPred == 'tabDenspred'",
             selectInput(label = labelInput("selvar"), inputId = ns("sel_dens_pred"), choices = "")
           )),
      list(
        conditionalPanel(
          "input.BoxPodPred == 'tabDistpredcat'",
          codigo.monokai(ns("fieldCodeDistpredcat"), height = "10vh")),
        conditionalPanel(
          "input.BoxPodPred == 'tabDenspred'",
          codigo.monokai(ns("fieldCodeDenspred"), height = "10vh")))
    ))))
  
  tagList(
    tabBoxPrmdt(
      id = "BoxPodPred",opciones = opc_podpred,
      tabPanel(
        title = labelInput("distpred"), value = "tabDistpred",
        withLoader(echarts4rOutput(ns('hc_distpred'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("pares"), value = "tabPares",
        withLoader(plotOutput(ns('plot_pairs_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("distpredcat"), value = "tabDistpredcat",
        withLoader(echarts4rOutput(ns('plot_dist_poder'), height = "75vh"), 
                   type = "html", loader = "loader4")),
      tabPanel(
        title = labelInput("denspred"), value = "tabDenspred",
        withLoader(echarts4rOutput(ns('plot_density_poder'), height = "75vh"), 
                   type = "html", loader = "loader4"))
    )
  )
}
    
#' poder_pred Server Function
#'
#' @noRd 

mod_poder_pred_server <- function(input, output, session, updateData){
  ns <- session$ns
 
  # Gráfico de Distribución Variable a Predecir 
  output$hc_distpred = renderEcharts4r({
    var  <- updateData$variable.predecir
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      data <- updateData$datos[, var]
      cod  <- code.dist.varpred(var)
      updateAceEditor(session, "fieldCodeDistpred", value = cod)
      label   <- levels(data) 
      color   <- gg_color_hue(length(levels(data)))
      value   <- summary(data, maxsum = length(levels(data)))
      prop    <- value/length(data)
      grafico <- data.frame (
        name  = var,
        label = label, 
        value = value,
        color = color,
        prop  = prop
      )

      grafico |> 
        e_charts(label) |> 
        e_bar(value, prop, name = var) |> 
        e_tooltip(formatter = e_JS(paste0("function(params){
                                      return('<strong>' +  params.value[0] +
                                             '</strong><br />", tr("porcentaje", updateData$idioma) ,": ' + parseFloat(params.name * 100).toFixed(2)+
                                             '%<br /> ' + '", tr("cant", updateData$idioma),": ' + params.value[1])}"))) |> 
        e_legend(FALSE)|> e_show_loading()|>        
        e_datazoom(show = F) |> e_add_nested("itemStyle", color)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  

  # Update on load testing data
  observeEvent(updateData$datos.prueba, {
    variable     <- updateData$variable.predecir
    datos        <- updateData$datos
    nombres      <- colnames.empty(var.numericas(datos))
    cat.sin.pred <- colnames.empty(var.categoricas(datos))
    cat.sin.pred <- cat.sin.pred[cat.sin.pred != variable]
    updateSelectInput(session, "sel_pred_cat", choices = cat.sin.pred)
    updateSelectInput(session, "sel_dens_pred", choices = nombres)
  })
  
  #Pairs Plot Output
  output$plot_pairs_poder <- renderPlot({
    tryCatch({
      variable  <- updateData$variable.predecir
      datos     <- updateData$datos
      cod.pairs <- code.pairs.poder(variable)
      idioma    <- updateData$idioma
      res       <-    NULL
      updateAceEditor(session, "fieldCodePares", value = cod.pairs)
       if (ncol(var.numericas(datos)) >= 2) {
         if(ncol(var.numericas(datos)) <= 25){
           pairs.poder(datos,variable)
         }else{
           showNotification(tr("bigPlot",idioma), duration = 10, type = "message")
           
         }
       }else{
         showNotification(paste0(tr("errornum",idioma)),
                          duration = 10,
                          type = "message")
         res <- NULL
       }
      return(res)
      
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
    
  })
  
  
  # Hace el gráfico de densidad de variables númericas
  output$plot_density_poder <- renderEcharts4r({
    variable.num  <- input$sel_dens_pred
    idioma        <- updateData$idioma
    variable.pred <- updateData$variable.predecir
    datos         <- updateData$datos
    
    tryCatch({

      if (ncol(var.numericas(datos)) >= 1) {
        cod <- paste0("e_numerico_dens(datos, '", variable.num,
                      "', '", variable.pred, "', label = '",tr("denspodlab",idioma) ,"' ))")
        updateAceEditor(session, "fieldCodeDenspred", value = cod)
        e_numerico_dens(datos, variable.num, variable.pred, label=tr("denspodlab", idioma))
      }else{#No retorna nada porque el grafico de error es con PLOT no ECHARTS4R
        showNotification(paste0(tr("errornum",idioma)),
                         duration = 10,
                         type = "message")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })
  
  # Hace el gráfico de poder predictivo categórico
  output$plot_dist_poder <- renderEcharts4r({
    variable.cat  <- input$sel_pred_cat
    idioma        <- updateData$idioma
    variable.pred <- updateData$variable.predecir
    datos         <- updateData$datos

    tryCatch({
      if (ncol(var.categoricas(datos)) > 1) {
        cod <- paste0("e_categorico_dist(datos, '", variable.cat,
                      "', '", variable.pred, "', label = '",tr("distpodcat",idioma) ,"' ))")
        updateAceEditor(session, "fieldCodeDistpredcat", value = cod)
        e_categorico_dist(datos, variable.cat, variable.pred, label=tr("distpodcat",idioma))
        
      }else{
        showNotification(paste0(tr("errorcat",idioma)),
                         duration = 10,
                         type = "message")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste0("Error en Poder Predictivo: ", e),
                       duration = 10,
                       type = "error")
      return(NULL)
    })
  })

}
    
## To be copied in the UI
# mod_poder_pred_ui("poder_pred_ui_1")
    
## To be copied in the server
# callModule(mod_poder_pred_server, "poder_pred_ui_1")
 
