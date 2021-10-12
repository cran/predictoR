#' distribuciones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribuciones_ui <- function(id){
  ns <- NS(id)
  
  titulo_dist <- tags$div(
    class = "multiple-select-var", conditionalPanel(
      condition = "input.tabDyA == 'numericas'",
      selectInput(inputId = ns("sel_dya_num"), label = NULL, choices = "")),
    conditionalPanel(
      condition = "input.tabDyA == 'categoricas'",
      selectInput(inputId = ns("sel_dya_cat"), label = NULL, choices = ""))
  )
  
  opc_dist <- fluidRow(
    conditionalPanel(
      "input.tabDyA == 'categoricas'",
      tabsOptions(botones = list(icon("code")), widths = 100,heights = 55, tabs.content = list(
        list(
          codigo.monokai(ns("fieldCodeCat"), height = "20vh"))
      ))),
    conditionalPanel(
      "input.tabDyA == 'numericas'",
      tabsOptions(botones = list(icon("cog"), icon("info"), icon("code")),
                  widths = c(50, 100, 100), heights = c(50, 50, 35), 
      tabs.content = list(
        list(
          options.base(), tags$hr(style = "margin-top: 0px;"),
          conditionalPanel(
            condition = "input.tabDyA == 'numericas'",
            colourpicker::colourInput(
              ns("col_dist_bar"), labelInput("selcolbar"), value = "steelblue", 
              allowTransparent = T),
            colourpicker::colourInput(
              ns("col_dist_point"), labelInput("selcolline"), value = "red",
              allowTransparent = T)
          )
        ),
        list(DT::dataTableOutput(ns("mostrar.atipicos"))),
        list(
          codigo.monokai(ns("fieldCodeNum"), height = "10vh"))
      ))))
  
  tagList(
    tabBoxPrmdt(
      id = "tabDyA", title = titulo_dist, opciones = opc_dist,
      tabPanel(
        title = labelInput("numericas"), value = "numericas",
        echarts4rOutput(ns('plot_num'), height = "75vh")),
      tabPanel(
        title = labelInput("categoricas"), value = "categoricas",
        echarts4rOutput(ns('plot_cat'), height = "75vh"))
    )
  )
}

#' distribuciones Server Function
#' @keywords internal
mod_distribuciones_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #' Update on load data
  observeEvent(updateData$datos, {
    datos       <- updateData$datos
    numericos   <- var.numericas(datos)
    categoricos <- var.categoricas(datos)
    
    updateSelectInput(session, "sel_dya_num", choices = colnames(numericos))
    updateSelectInput(session, "sel_dya_cat", choices = colnames(categoricos))
  })
  
  #' Gráfico de Distribuciones (Númericas)
  output$plot_num = renderEcharts4r({
    datos      <- updateData$datos
    var        <- input$sel_dya_num
    colorBar   <- input$col_dist_bar
    colorPoint <- input$col_dist_point
    titulos <- c(
      tr("minimo", updateData$idioma),
      tr("q1", updateData$idioma),
      tr("mediana", updateData$idioma),
      tr("q3", updateData$idioma),
      tr("maximo", updateData$idioma)
    )
    
    tryCatch({
      cod <- paste0("e_histboxplot(datos[['", var, "']], '", var, "', '", 
                    colorBar, "', '", colorPoint, "', c('", 
                    paste(titulos, collapse = "', '"), "'))\n")
      updateAceEditor(session, "fieldCodeNum", value = cod)
      e_histboxplot(datos[[var]], var, colorBar, colorPoint, titulos)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    datos <- updateData$datos
    var   <- input$sel_dya_num
    atipicos <- boxplot.stats(datos[, var])
    datos <- datos[datos[, var] %in% atipicos$out, var, drop = F]
    datos <- datos[order(datos[, var]), , drop = F]
    DT::datatable(datos, options = list(
      dom = 't', scrollX = TRUE, scrollY = "28vh", pageLength = nrow(datos))) |>
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })
  
  #' Gráfico de Distribuciones (Categóricas)
  output$plot_cat = renderEcharts4r({
    var  <- input$sel_dya_cat
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      datos.plot <- updateData$datos[, var]
      
      cod <- code.dist.cat(var)
      updateAceEditor(session, "fieldCodeCat", value = cod)
      
      label <- levels(datos.plot)
      value <- summary(datos.plot, maxsum = length(levels(datos.plot)))
      color <- gg_color_hue(length(label))
      datos.plot <- data.frame (
        label = label,
        value = value,
        color = color
      )
      
      datos.plot |> e_charts(label) |> e_bar(value, name = var) |>
        e_tooltip() |> e_datazoom(show = F) |> e_show_loading()|>
        e_add_nested("itemStyle", color)|> e_legend(FALSE)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
}

## To be copied in the UI
# mod_distribuciones_ui("distribuciones_ui_1")

## To be copied in the server
# callModule(mod_distribuciones_server, "distribuciones_ui_1")

