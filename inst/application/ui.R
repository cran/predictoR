
suppressMessages(suppressWarnings({
  library(DT)
  library(ada)
  library(kknn)
  library(tidyverse)
  library(shiny)
  library(e1071)
  library(rpart)
  library(rpart.plot)
  library(ROCR)
  library(knitr)
  library(glmnet)
  library(rattle)
  library(xtable)
  library(xgboost)
  library(shinyjs)
  library(ggplot2)
  library(stringr)
  library(forcats)
  library(shinyAce)
  library(corrplot)
  library(neuralnet)
  library(randomForest)
  library(colourpicker)
  library(shinyWidgets)
  library(flexdashboard)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(dplyr)
}))

# FUNCIONES --------------------------------------------------------------------------------------------------------------

execute.btn <- function (runid){
  tags$div(style = "text-align:right;padding-right: 10px;",
                        tags$button(id = runid, type = "button", class = "run-button action-button",
                                    icon("play"), tags$a(labelInput("ejecutar"), style = "color:white")))
}

# Crea un campo de codigo con boton de ejecutar y cargar
campo.codigo <- function(fieldid, ...) {
  tags$div(class = "box box-solid bg-black",
           tags$div(class = "box-body",
                    aceEditor(fieldid, mode = "r", theme = "monokai",readOnly = T, value = "", ...)))
}

# Crea un cuadro para la inforación (ver página de información)
infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(class = "info-box bg-promidat",
           tags$span(class = "info-box-icon", icono),
           tags$div(class="info-box-content",
                    tags$span(class = "info-box-text", titulo),
                    tags$span(class = "info-box-number", valor)))
}

# Un wrapper de texto que permite cambiar el idioma
labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

inputRadio <- function(inputId, value, isSelected) {
  res <- tags$input(type="radio", name=inputId, value=value)
  if(isSelected){
    res$attribs$checked <- "checked"
  }
  return(res)
}

# Crea una radioButtons que puede cambiar de idioma
radioButtonsTr <- function(inputId, label, values, names){
  item <- function(i){
    tags$div(class="radio",tags$label(inputRadio(inputId, values[i], i == 1),tags$span(labelInput(names[i]))))
  }
  tags$div(id=inputId, class="form-group shiny-input-radiogroup shiny-input-container",
           tags$label(class="control-label", `for`= inputId, labelInput(label)),
           tags$div(class="shiny-options-group", lapply(1:length(values), item )))
}

#Genera los campos para las opciones y parametros
tabsOptions <- function(botones = list(icon("gear"), icon("terminal")), widths = c(50, 100),
                        heights = c(100, 50), tabs.content = list("", "")){
  res <- ""
  codeButtons <- ""
  cant <- length(botones)
  if(cant == 1) {widgets <- c("center")}
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for(i in 1:cant){
    res <- paste0(res, tags$div(class = paste0("box-option box-option-", widgets[i]),
                                style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
                                tabs.content[[i]]), "\n")
    codeButtons <- paste0(codeButtons, "<button style='width:", 100/cant, "%' data-widget='",
                          widgets[i], "'>", botones[[i]], "</button>\n")
  }
  res <- paste0(res, tags$div(class = "btn-options", style = "position:relative;",width = "100%", HTML(codeButtons)))
  return(tags$div(HTML(res)))
}


# MENU --------------------------------------------------------------------------------------------------------------------

menu.cargar <- menuItem(labelInput("data"), tabName = "cargar", icon = icon("dashboard"))

menu.estadisticas <- menuItem(labelInput("basico"), tabName = "parte1", icon = icon("th-list"),
                              menuSubItem(labelInput("resumen"), tabName = "resumen", icon = icon("sort-numeric-asc")),
                              menuSubItem(labelInput("normalidad"), tabName = "normalidad", icon = icon("bar-chart")),
                              menuSubItem(labelInput("dispersion"), tabName = "dispersion", icon = icon("line-chart")),
                              menuSubItem(labelInput("distribucion"), tabName = "distribucion", icon = icon("area-chart")),
                              menuSubItem(labelInput("correlacion"), tabName = "correlacion", icon = icon("table")),
                              menuItem(labelInput("poderpred"), tabName = "poderPred", icon = icon("rocket")))

menu.aprendizaje.supervisado <- menuItem(labelInput("aprendizaje"), tabName = "parte2", icon = icon("th-list"),
                                         menuSubItem(labelInput("knnl"),tabName = "knn",icon = icon("dot-circle-o")),
                                         menuSubItem(labelInput("dtl"),tabName = "dt",icon = icon("tree")),
                                         menuSubItem(labelInput("rfl"),tabName = "rf",icon = icon("sitemap")),
                                         menuSubItem(labelInput("bl"),tabName = "boosting",icon = icon("superscript")),
                                         menuSubItem(labelInput("svml"),tabName = "svm",icon = icon("line-chart")),
                                         menuSubItem("Bayes",tabName = "bayes",icon = icon("dice")),
                                         menuSubItem(labelInput("nn"),tabName = "nn",icon = icon("brain")),
                                         menuSubItem(labelInput("xgb"),tabName = "xgb",icon = icon("project-diagram")),
                                         menuSubItem(labelInput("rl"),tabName = "rl",icon = icon("line-chart")),
                                         menuSubItem(labelInput("rlr"),tabName = "rlr",icon = icon("line-chart")))

menu.reporte <- menuItem(labelInput("reporte"), tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))

menu.comparar <- menuItem(labelInput("comparacion"), tabName = "comparar", icon = icon("eye"))

menu.prediccion.nuevos <- menuItem(labelInput("predicnuevos"), tabName = "predNuevos", icon = icon("table"))

menu.info <- menuItem(labelInput("acercade"), tabName = "acercaDe", icon = icon("info"))

menu.idioma <- tags$li(class = "nodisabled treeview",
                       tags$a(href = "#shiny-tab-tabdioma",
                              tags$i(class="fa fa-language"),
                              labelInput("idioma"),
                              tags$i(class="fa fa-angle-left pull-right")),
                       tags$ul(class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
                              radioButtons('idioma', labelInput("selidioma"), c('Español'='es', 'English'='en')),
                              tags$br()))

#Los sliderInput y colourpicker por un motivo imprevisto se tienen que inicializar
#De lo contrario no se vana mostrar en algunas partes de la interfaz
init.inputs <- tags$div(style = "display:none;",
                          sliderInput(inputId = "aux", min = 2, value = 2,
                                      label = "Cantidad de Clusters", max = 10),
                          colourpicker::colourInput(
                            "auxColor", NULL, value = "red", allowTransparent = T))

mi.menu <- sidebarMenu(id = "principal",
              tags$div(id = "espacioMenu"),
              menu.cargar,
              menu.estadisticas,
              menu.aprendizaje.supervisado,
              menu.comparar,
              menu.prediccion.nuevos,
              menu.reporte,
              menu.info,
              hr(),
              menu.idioma,
              init.inputs)

# HEAD HTML ---------------------------------------------------------------------------------------------------------

#Importa css y js. Tambien se decide el icono
mi.head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style_predictor.css"),
  tags$link(rel="icon", href="http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
  useShinyjs(),
  tags$script(src = "script_predictor.js"))

#La  pagina de carga
load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy'))",
                              div(id = "loaderWrapper", div(id="loader")))

# PAGINA DE CARGA Y TRANSFORMACION DE DATOS -----------------------------------------------------------------------------

panel.cargar.datos <- tabPanel(title = labelInput("cargar"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               checkboxInput('header', labelInput("header"), TRUE),
                               checkboxInput('rowname', labelInput("Rownames"), TRUE),
                               radioButtonsTr('sep', "separador", c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                               radioButtonsTr('dec', "separadordec", c(',', '.'), c("coma", "punto")),
                               switchInput(inputId = "deleteNA", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                               fileInput('file1', label =  labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton("loadButton", labelInput("cargar"), width = "100%"),
                               br(),br(),
                               aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "13vh", readOnly = T))

panel.tansformar.datos <- tabPanel(title = labelInput("trans"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   DT::dataTableOutput('transData'),
                                   br(),br(),
                                   actionButton("transButton", labelInput("aplicar"), width = "100%"),
                                   br(),br(),
                                   aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

panel.segmentar.datos <- tabPanel(title = labelInput("configuraciones"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                  fluidRow(column(id = "colSemilla",width = 6, numericInput("semilla", labelInput("semilla"), value = "", width = "100%")), br(),
                                           column(width = 6, switchInput(inputId = "permitir.semilla", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                         label = "", onLabel = labelInput("habilitada"), offLabel = labelInput("deshabilitada"), labelWidth = "100%",
                                                                         inline = T,size = "large"))),
                                  selectInput(inputId = "sel.predic.var", label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                                  sliderInput("segmentacionDatosA", labelInput("propA"),width = "100%",
                                              min = 5, max = 95, value = 70, step = 5),
                                  sliderInput("segmentacionDatosT", labelInput("propP"), width = "100%",
                                              min = 5, max = 95, value = 30, step = 5),
                                  actionButton("segmentButton", labelInput("generar"), width = "100%"),
                                  br(),br(),
                                  aceEditor("fieldCodeSegment", mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))

muestra.datos <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                     DT::DTOutput('contents'), hr(),
                     downloadButton("downloaDatos", labelInput("descargar"), width = "100%"))

muestra.datos.aprend <- box(title = labelInput("dataA"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                            DT::DTOutput('contentsAprend'), hr(),
                            downloadButton("downloaDatosA", labelInput("descargar"), width = "100%"))

muestra.datos.prueba <- box(title = labelInput("dataP"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                            DT::DTOutput('contentsPrueba'), hr(),
                            downloadButton("downloaDatosP", labelInput("descargar"), width = "100%"))

pagina.cargar.datos <- tabItem(tabName = "cargar",
                               fluidRow(column(width = 5, tabBox(id ="tabs", title = NULL, width = 12,
                                                                 panel.cargar.datos,
                                                                 panel.tansformar.datos,
                                                                 panel.segmentar.datos)),
                                        column(width = 7, muestra.datos)),
                               conditionalPanel(condition = paste0("input.tabs == '", labelInput("configuraciones"),"'"), #OJO AQUI
                                                fluidRow(column(width = 6, muestra.datos.aprend),
                                                         column(width = 6, muestra.datos.prueba))) )

# PAGINA DE RESUMEN NUMERICO ----------------------------------------------------------------------------------------------

cuadro.resumen.completo <- box(title = labelInput("resumen"), status = "primary", width = 7, solidHeader = TRUE, collapsible = TRUE,
                               DT::dataTableOutput("resumen.completo"), hr(),
                               aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "5vh",  readOnly = T))

cuadro.resumen.variable <- box(title = labelInput("resumenvar"), status = "primary", width = 5, solidHeader = TRUE, collapsible = TRUE,
                               selectInput(inputId = "sel.resumen", label = labelInput("selvar"), choices =  ""),
                               fluidRow(uiOutput("resumen")))

pagina.resumen.numerico <- tabItem(tabName = "resumen",
                                   fluidRow(cuadro.resumen.completo,
                                   cuadro.resumen.variable ))

# PAGINA DEL TEST DE NORMALIDAD -------------------------------------------------------------------------------------------

panel.grafico.normalidad.num <- tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot", plotOutput('plot.normal', height = "65vh"))

panel.grafico.normalidad.cat <- tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc", DT::dataTableOutput('calculo.normal'))

boton.colores <- list(h4(labelInput("opciones")), hr(),
                      colourpicker::colourInput("col.normal", labelInput("selcolor"),value = "#00FF22AA", allowTransparent = T))

codigo.normalidad <- list(h4(labelInput("codigo")), hr(),
                          conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                           campo.codigo("fieldCalcNormal", height = "20vh")),
                          conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                           campo.codigo("fieldCodeNormal", height = "25vh")))

tabs.normal <- tabsOptions(heights = c(33, 63), tabs.content = list(boton.colores, codigo.normalidad))

opciones.normalidad <-  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.normal", label = NULL, choices =  ""))

pagina.test.normalidad <- tabItem(tabName = "normalidad",
                                  tabBox(id = "BoxNormal",
                                         width = 12, title = opciones.normalidad,
                                         panel.grafico.normalidad.num,
                                         panel.grafico.normalidad.cat,
                                         tabs.normal))

# PAGINA DE DISPERSION -----------------------------------------------------------------------------------------------------

tabs.dispersion  <-  tabsOptions(heights = c(30, 39),
                                 tabs.content = list(list(h4(labelInput("opciones")), hr(),
                                                     colourpicker::colourInput("col.disp", labelInput("selcolor"),
                                                                               value = "#FF0000AA",allowTransparent = T)),
                                                     list(h4(labelInput("codigo")), hr(),
                                                          column(width = 12, campo.codigo("fieldCodeDisp", height = "7vh")))))

codigo.dispersion <- column(width = 12, campo.codigo(fieldid = "fieldCodeDisp", height = "8vh"))

datos.dispersiones <- column(width = 4, DT::dataTableOutput('mostrar.disp.zoom'), hr(), plotOutput('plot.disp.zoom', height = "41vh"))

opciones.dispersion <- fluidRow(h4(style = "float:left;font-size: 20px;", labelInput("selvars")),
                                tags$div(class="multiple-select-var",style = "width:60%;",
                                         selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                        options = list(maxItems = 3))))

grafico.dispersion <- tabPanel(title = labelInput("dispersion"), value = "tabDisp",
                               fluidRow(column(width = 8, plotOutput('plot.disp', height = "65vh",
                                        brush = brushOpts(id = "zoom.disp", resetOnNew = TRUE))),
                                        datos.dispersiones))

pagina.dispersion<- tabItem(tabName = "dispersion",
                            tabBox(id = "BoxDisp", width = NULL, title = opciones.dispersion,
                                   grafico.dispersion,
                                   tabs.dispersion))

# PAGINA DE DISTRIBUCIONES -------------------------------------------------------------------------------------------------

opciones.distribuciones <- list(h4(labelInput("opciones")), hr(), colourpicker::colourInput("col.dist", labelInput("selcolor"), value = "#FF0000AA", allowTransparent = T))

campos.codigos.distribuciones <- list(h4(labelInput("codigo")), hr(),
                                      conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                       campo.codigo("fieldCodeNum", height = "7vh")),
                                      conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                       campo.codigo("fieldCodeCat", height = "7vh")))

codigo.distribuciones <- list(h4(labelInput("codigo")), hr(),
                              tabBox(id = "tabCodeDyA", width = NULL, title = labelInput("codedist"),
                                     tabPanel(title = labelInput("numericas"),
                                              aceEditor("fieldFuncNum",mode = "r",theme = "monokai",value = "",height = "285px",readOnly = T)),
                                     tabPanel(title = labelInput("categoricas"),
                                              aceEditor("fieldFuncCat",mode = "r",theme = "monokai",value = "",height = "165px",readOnly = T))))

tabs.distribuciones <- tabsOptions(botones = list(icon("gear"), icon("terminal"), icon("info"), icon("code")),
                                   widths = c(50, 100, 100, 100), heights = c(30, 35, 48, 80),
                                   tabs.content = list(opciones.distribuciones,
                                                       campos.codigos.distribuciones,
                                                       list(DT::dataTableOutput("mostrar.atipicos")),
                                                       codigo.distribuciones))

selector.variables.distribucion <- tags$div(class = "multiple-select-var",
                                            conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                             selectInput(inputId = "sel.distribucion.num",label = NULL,choices =  "")),
                                            conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                             selectInput(inputId = "sel.distribucion.cat",label = NULL,choices =  "")))

resultados.distribucion.numericas <- tabPanel(title = labelInput("numericas"), value = "numericas",
                                              plotOutput('plot.num', height = "70vh"),
                                              actionButton(inputId="distribucion_numerica",label = ""))

resultados.distribucion.categoricas <- tabPanel(title = labelInput("categoricas"), value = "categoricas",plotOutput('plot.cat', height = "70vh"))

pagina.distribuciones <- tabItem(tabName = "distribucion",
                                 tabBox(id = "tabDyA", width = NULL,
                                        title =  selector.variables.distribucion,
                                        resultados.distribucion.numericas,
                                        resultados.distribucion.categoricas,
                                        tabs.distribuciones))

# PAGINA DE CORRELACIONES -------------------------------------------------------------------------------------------------

opciones.cor <-list(h4(labelInput("opciones")), hr(),
                    selectInput(inputId = "cor.metodo", label = labelInput("selmetodo"),
                                choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                    selectInput(inputId = "cor.tipo", label = labelInput("seltipo"), choices =  c("lower", "upper", "full")))

codigo.cor <- list(h4(labelInput("codigo")), hr(),
                   aceEditor("fieldModelCor", height = "6vh", mode = "r", theme = "monokai", value = "", readOnly = T),
                   campo.codigo("fieldCodeCor", height = "7vh"))

opciones.correlaciones <- tabsOptions(heights = c(48, 53),
                                      tabs.content = list(opciones.cor, codigo.cor))

tab.correlacion <- tabPanel(title = labelInput("correlacion"), value = "correlacion", plotOutput('plot.cor', height = "70vh"))

tab.resultados.correlaciones <- tabPanel(title = labelInput("resultados"), value = "cor.salida", verbatimTextOutput("txtcor"))

pagina.correlaciones <- tabItem(tabName = "correlacion",
                                tabBox(id = "tabCor", width = NULL,
                                       tab.correlacion,
                                       tab.resultados.correlaciones,
                                       opciones.correlaciones))

# PAGINA DE PODER PREDICTIVO ----------------------------------------------------------------------------------------------

codigo.poder.uno <- list(h4(labelInput("codigo")), hr(),
                         conditionalPanel("input.BoxPodPred == 'preddist'",
                                          campo.codigo(fieldid = "fieldCodePoderPred", height = "16vh")),
                         conditionalPanel("input.BoxPodPred == 'predpares'",
                                           campo.codigo(fieldid = "fieldCodePoderNum", height = "16vh")))


codigo.poder.comp<- list(h4(labelInput("codigo")), hr(),
                         conditionalPanel("input.BoxPodPred == 'predcatdist'",
                                          campo.codigo(fieldid = "fieldCodePoderCat", height = "38vh")),
                         conditionalPanel("input.BoxPodPred == 'preddens'",
                                             campo.codigo(fieldid = "fieldCodePoderDens", height = "16vh")))

opciones.poder.comp<- list(h4(labelInput("opciones")), hr(),
                           conditionalPanel("input.BoxPodPred == 'predcatdist'",
                                            selectInput(inputId = "sel.distribucion.poder", label = NULL, choices =  "", width = "100%")),
                           conditionalPanel("input.BoxPodPred == 'preddens'",
                                            selectInput(inputId = "sel.density.poder", label = NULL, choices =  "", width = "100%")))


tabs.poder.predit.uno <- conditionalPanel("input.BoxPodPred == 'preddist' || input.BoxPodPred == 'predpares'",
                                          tabsOptions(botones = list(icon("terminal")), widths = 100, heights = 55,
                                                      tabs.content = list(codigo.poder.uno)))

tabs.poder.predit.dos <- conditionalPanel("input.BoxPodPred == 'predcatdist' || input.BoxPodPred == 'preddens'",
                                          tabsOptions(botones = list(icon("gear"),icon("terminal")), widths = c(50,100), heights = c(40, 93),
                                                      tabs.content = list(opciones.poder.comp, codigo.poder.comp)))

plot.pred.poder <- tabPanel(title = labelInput('distpred'), value = "preddist",
                            plotOutput('plot.pred.poder', height = "55vh"))

plot.dist.poder <- tabPanel(title = labelInput('distpredcat'), value = "predcatdist",
                            plotOutput('plot.dist.poder', height = "55vh"))

plot.pairs.poder <- tabPanel(title = labelInput('pares'), value = "predpares",
                             plotOutput('plot.pairs.poder', height = "55vh"))

plot.dens.poder <- tabPanel(title = labelInput('denspred'), value = "preddens",
                             plotOutput('plot.density.poder', height = "55vh"))


pagina.poder <- tabItem(tabName = "poderPred",
                        tabBox(id = "BoxPodPred", width = NULL,
                               plot.pred.poder,
                               plot.pairs.poder,
                               plot.dist.poder,
                               plot.dens.poder,
                               tabs.poder.predit.uno,
                               tabs.poder.predit.dos))

# PAGINA DE KNN -----------------------------------------------------------------------------------------------------------

opciones.knn <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,execute.btn("runKnn"))),
                     hr(),
                     fluidRow(column(numericInput("kmax.knn", labelInput("kmax"), min = 1,step = 1, value = 7), width = 6),
                              column(selectInput(inputId = "kernel.knn", label = labelInput("selkernel"),selected = 1,
                                     choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                 "triweight", "cos","inv","gaussian")),width = 6)),
                     fluidRow(column(switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                     label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=7)))

codigo.knn <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxKnn == 'tabKknModelo'",
                                    aceEditor("fieldCodeKnn", mode = "r", theme = "monokai", value = "", height = "4vh", readOnly = T)),
                   conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                    aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxKnn == 'tabKknMC'",
                                    aceEditor("fieldCodeKnnMC", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                    aceEditor("fieldCodeKnnIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.knn <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(opciones.knn, codigo.knn))

panel.generar.knn <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                             verbatimTextOutput("txtknn"))

panel.prediccion.knn <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                 DT::dataTableOutput("knnPrediTable"))

panel.matriz.confucion.knn <- tabPanel(title = labelInput("mc"), value = "tabKknMC",
                                       plotOutput('plot.knn.mc', height = "45vh"),
                                       verbatimTextOutput("txtknnMC"))

panel.indices.generales.knn <- tabPanel(title = labelInput("indices"), value = "tabKknIndex",
                                    fluidRow(column(width = 6, gaugeOutput("knnPrecGlob", width = "100%")),
                                             column(width = 6, gaugeOutput("knnErrorGlob", width = "100%"))),
                                    fluidRow(column(width = 12, shiny::tableOutput("knnIndPrecTable"))),
                                    fluidRow(column(width = 12, shiny::tableOutput("knnIndErrTable"))))

pagina.knn <- tabItem(tabName = "knn",
                      tabBox(id = "BoxKnn", width = NULL, height ="80%",
                             panel.generar.knn,
                             panel.prediccion.knn,
                             panel.matriz.confucion.knn,
                             panel.indices.generales.knn,
                             tabs.knn))

# PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

opciones.svm <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,execute.btn("runSvm"))),
                     hr(),
                     conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                      fluidRow(column(br(),switchInput(inputId = "switch.scale.svm", onStatus = "success", offStatus = "danger", value = T,
                                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width = 6),
                                              column(selectInput(inputId = "kernel.svm", label = labelInput("selkernel"), selected = "radial",
                                                                 choices =  c("linear", "polynomial", "radial", "sigmoid")), width=6))),
                     conditionalPanel("input.BoxSvm == 'tabSvmPlot'",
                                      selectizeInput("select.var.svm.plot",NULL,label = "Variables Predictoras:", multiple = T, choices = c(""),
                                                     options = list(maxItems = 2, placeholder = ""), width = "100%")))

codigo.svm <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxSvm == 'tabSvmModelo'",
                                    aceEditor("fieldCodeSvm", mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmPlot'",
                                    aceEditor("fieldCodeSvmPlot", mode = "r", theme = "monokai",
                                              value = "", height = "6vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                    aceEditor("fieldCodeSvmPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmMC'",
                                    aceEditor("fieldCodeSvmMC", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                    aceEditor("fieldCodeSvmIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.svm <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(60, 95),
                        tabs.content = list(opciones.svm, codigo.svm))

panel.generar.svm <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                              verbatimTextOutput("txtSvm"))

plot.svm <- tabPanel(title = labelInput("gclasificacion"), value = "tabSvmPlot",
                     plotOutput('plot.svm', height = "55vh"))

panel.prediccion.svm <- tabPanel(title = labelInput("predm"), value = "tabSvmPred",
                                 DT::dataTableOutput("svmPrediTable"))

panel.matriz.confucion.svm <- tabPanel(title = labelInput("mc"), value = "tabSvmMC",
                                       plotOutput('plot.svm.mc', height = "45vh"),
                                       verbatimTextOutput("txtSvmMC"))

panel.indices.generales.svm <- tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
                                        fluidRow(column(width = 6, gaugeOutput("svmPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("svmErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("svmIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("svmIndErrTable"))))

pagina.svm <- tabItem(tabName = "svm",
                      tabBox(id = "BoxSvm", width = NULL, height ="80%",
                             panel.generar.svm,
                             plot.svm,
                             panel.prediccion.svm,
                             panel.matriz.confucion.svm,
                             panel.indices.generales.svm,
                             tabs.svm))

# PAGINA DE DT ------------------------------------------------------------------------------------------------------------

opciones.dt <- list(fluidRow(column(width = 9, h4(labelInput("opciones"))),
                              column(width = 2,execute.btn("runDt"))),
                     hr(),
                     fluidRow(column(numericInput("minsplit.dt", labelInput("minsplit"), 2, width = "100%",min = 1), width = 6),
                              column(numericInput("maxdepth.dt", labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1),width = 6)),
                     fluidRow(column(selectInput(inputId = "split.dt", label = labelInput("splitIndex"),selected = 1,
                                                 choices =  list("gini" = "gini", "Entropía" = "information")), width=12)))

codigo.dt <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxDt == 'tabDtModelo'",
                                    aceEditor("fieldCodeDt", mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = T, autoComplete = "enabled")),
                  conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                   aceEditor("fieldCodeDtPlot", mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxDt == 'tabDtPred'",
                                    aceEditor("fieldCodeDtPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxDt == 'tabDtMC'",
                                    aceEditor("fieldCodeDtMC", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                    aceEditor("fieldCodeDtIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")),
                  conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                   aceEditor("fieldCodeDtRule", mode = "r", theme = "monokai",
                                             value = "", height = "4vh", readOnly = T, autoComplete = "enabled")))

tabs.dt <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(opciones.dt, codigo.dt))

panel.generar.dt <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                             verbatimTextOutput("txtDt"))

plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                     plotOutput('plot.dt', height = "55vh"))

panel.prediccion.dt <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                 DT::dataTableOutput("dtPrediTable"))

panel.matriz.confucion.dt <- tabPanel(title = labelInput("mc"), value = "tabDtMC",
                                      plotOutput('plot.dt.mc', height = "45vh"),
                                      verbatimTextOutput("txtDtMC"))

panel.indices.generales.dt <- tabPanel(title = labelInput("indices"),value = "tabDtIndex",
                                       fluidRow(column(width = 6, gaugeOutput("dtPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("dtErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("dtIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("dtIndErrTable"))))

panel.reglas.dt <- tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
                            verbatimTextOutput("rulesDt"))

pagina.dt <- tabItem(tabName = "dt",
                     tabBox(id = "BoxDt", width = NULL, height ="80%",
                            panel.generar.dt,
                            plot.dt,
                            panel.prediccion.dt,
                            panel.matriz.confucion.dt,
                            panel.indices.generales.dt,
                            panel.reglas.dt,
                            tabs.dt))

# PAGINA DE RF ------------------------------------------------------------------------------------------------------------

opciones.rf <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                             column(width = 2,execute.btn("runRf"))),
                    hr(),
                    conditionalPanel("input.BoxRf != 'tabRfRules'",
                                      fluidRow(column(numericInput("ntree.rf", labelInput("numTree"), 20, width = "100%", min = 0), width = 6),
                                               column(numericInput("mtry.rf",labelInput("numVars"),1, width = "100%", min = 1), width=6))),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      numericInput("rules.rf.n",labelInput("ruleNumTree"),1, width = "100%", min = 1)))

codigo.rf  <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxRf == 'tabRfModelo'",
                                    aceEditor("fieldCodeRf", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRferror'",
                                    aceEditor("fieldCodeRfPlotError", mode = "r", theme = "monokai",
                                              value = "", height = "5vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfImp'",
                                    aceEditor("fieldCodeRfPlot", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfPred'",
                                    aceEditor("fieldCodeRfPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfMC'",
                                    aceEditor("fieldCodeRfMC", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfIndex'",
                                    aceEditor("fieldCodeRfIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfRules'",
                                    aceEditor("fieldCodeRfRules", mode = "r", theme = "monokai",
                                              value = "", height = "4vh", readOnly = T, autoComplete = "enabled")))

tabs.rf  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(65, 95),
                        tabs.content = list(opciones.rf, codigo.rf))

panel.generar.rf <- tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
                             verbatimTextOutput("txtRf"))

plor.error.ft <- tabPanel(title = labelInput("evolerror"), value = "tabRferror",
                          plotOutput('plot.error.rf', height = "55vh"))

plot.rf <- tabPanel(title = labelInput("varImp"), value = "tabRfImp",
                    plotOutput('plot.rf', height = "55vh"))

panel.prediccion.rf <- tabPanel(title = labelInput("predm"), value = "tabRfPred",
                                DT::dataTableOutput("rfPrediTable"))

panel.matriz.confucion.rf <- tabPanel(title = labelInput("mc"), value = "tabRfMC",
                                      plotOutput('plot.rf.mc', height = "45vh"),
                                      verbatimTextOutput("txtRfMC"))

panel.indices.generales.rf <- tabPanel(title = labelInput("indices"), value = "tabRfIndex",
                                       fluidRow(column(width = 6, gaugeOutput("rfPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("rfErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rfIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rfIndErrTable"))))


reglas.rf <- tabPanel(title = labelInput("reglas"), value = "tabRfRules",
                      verbatimTextOutput("rulesRf"))

pagina.rf <- tabItem(tabName = "rf",
                     tabBox(id = "BoxRf", width = NULL, height ="80%",
                            panel.generar.rf,
                            plor.error.ft,
                            plot.rf,
                            panel.prediccion.rf,
                            panel.matriz.confucion.rf,
                            panel.indices.generales.rf,
                            reglas.rf,
                            tabs.rf))

# PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

opciones.b <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                            column(width = 2,execute.btn("runBoosting"))),
                   hr(),
                   conditionalPanel("input.BoxB != 'tabBRules'",
                                    fluidRow(column(numericInput("iter.boosting", labelInput("numTree"), 50, width = "100%",min = 1), width = 6),
                                             column(numericInput("maxdepth.boosting", labelInput("maxdepth"), 15, width = "100%",min = 1), width=6)),
                                    fluidRow(column(selectInput(inputId = "tipo.boosting", label = labelInput("selectAlg"),selected = 1,
                                                                choices =  c("discrete", "real", "gentle")), width = 6),
                                             column(numericInput("minsplit.boosting", labelInput("minsplit"), 20, width = "100%",min = 1), width=6))),
                   conditionalPanel("input.BoxB == 'tabBRules'",
                                    numericInput("rules.b.n",labelInput("ruleNumTree"),1, width = "100%", min = 1)))

codigo.b  <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxB == 'tabBModelo'",
                                    aceEditor("fieldCodeBoosting", mode = "r", theme = "monokai",
                                              value = "", height = "5vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBError'",
                                    aceEditor("fieldCodeBoostingPlot", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBImp'",
                                    aceEditor("fieldCodeBoostingPlotImport", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBPred'",
                                    aceEditor("fieldCodeBoostingPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBMC'",
                                    aceEditor("fieldCodeBoostingMC", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBIndex'",
                                    aceEditor("fieldCodeBoostingIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")),
                  conditionalPanel("input.BoxB == 'tabBRules'",
                                   aceEditor("fieldCodeBoostingRules", mode = "r", theme = "monokai",
                                             value = "", height = "4vh", readOnly = T, autoComplete = "enabled")))

tabs.b  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(63, 95),
                        tabs.content = list(opciones.b, codigo.b))

panel.generar.boosting <- tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                              verbatimTextOutput("txtBoosting"))

plot.boosting <- tabPanel(title = labelInput("evolerror"), value = "tabBError",
                                 plotOutput('plot.boosting', height = "55vh"))

plot.boosting.import <- tabPanel(title = labelInput("varImp"), value = "tabBImp",
                    plotOutput('plot.boosting.import', height = "55vh"))

panel.prediccion.boosting <- tabPanel(title = labelInput("predm"), value = "tabBPred",
                                 DT::dataTableOutput("boostingPrediTable"))

panel.matriz.confucion.boosting <- tabPanel(title = labelInput("mc"), value = "tabBMC",
                                       plotOutput('plot.boosting.mc', height = "45vh"),
                                       verbatimTextOutput("txtBoostingMC"))

panel.indices.generales.boosting <- tabPanel(title = labelInput("indices"),value = "tabBIndex",
                                        fluidRow(column(width = 6, gaugeOutput("boostingPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("boostingErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("boostingIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("boostingIndErrTable"))))

reglas.boosting <- tabPanel(title = labelInput("reglas"), value = "tabBRules",
                            verbatimTextOutput("rulesB"))

pagina.boosting <- tabItem(tabName = "boosting",
                           tabBox(id = "BoxB", width = NULL, height ="80%",
                                  panel.generar.boosting,
                                  plot.boosting,
                                  plot.boosting.import,
                                  panel.prediccion.boosting,
                                  panel.matriz.confucion.boosting,
                                  panel.indices.generales.boosting,
                                  reglas.boosting,
                                  tabs.b))

# PAGINA DE BAYES ---------------------------------------------------------------------------------------------------------

codigo.bayes <- list(fluidRow(column(width = 11,h4(labelInput("codigo"))),
                              column(width = 1,execute.btn("runBayes"))),
                     hr(),
                     conditionalPanel("input.BoxBayes == 'tabBayesModelo'",
                                      aceEditor("fieldCodeBayes", mode = "r", theme = "monokai", value = "", height = "4vh", readOnly = T)),
                     conditionalPanel("input.BoxBayes == 'tabBayesPred'",
                                      aceEditor("fieldCodeBayesPred", mode = "r", theme = "monokai",
                                                value = "", height = "3vh", readOnly = T, autoComplete = "enabled")),
                     conditionalPanel("input.BoxBayes == 'tabBayesMC'",
                                      aceEditor("fieldCodeBayesMC", mode = "r", theme = "monokai",
                                                value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                     conditionalPanel("input.BoxBayes == 'tabBayesIndex'",
                                      aceEditor("fieldCodeBayesIG", mode = "r", theme = "monokai",
                                                value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.bayes <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                          tabs.content = list(codigo.bayes))

panel.generar.bayes <- tabPanel(title = labelInput("generatem"), value = "tabBayesModelo",
                                verbatimTextOutput("txtbayes"))

panel.prediccion.bayes <- tabPanel(title = labelInput("predm"), value = "tabBayesPred",
                                   DT::dataTableOutput("bayesPrediTable"))

panel.matriz.confucion.bayes <- tabPanel(title = labelInput("mc"), value = "tabBayesMC",
                                        plotOutput('plot.bayes.mc', height = "45vh"),
                                        verbatimTextOutput("txtbayesMC"))

panel.indices.generales.bayes <- tabPanel(title = labelInput("indices"), value = "tabBayesIndex",
                                         fluidRow(column(width = 6, gaugeOutput("bayesPrecGlob", width = "100%")),
                                                  column(width = 6, gaugeOutput("bayesErrorGlob", width = "100%"))),
                                         fluidRow(column(width = 12, shiny::tableOutput("bayesIndPrecTable"))),
                                         fluidRow(column(width = 12, shiny::tableOutput("bayesIndErrTable"))))

pagina.bayes <- tabItem(tabName = "bayes",
                      tabBox(id = "BoxBayes", width = NULL, height ="80%",
                             panel.generar.bayes,
                             panel.prediccion.bayes,
                             panel.matriz.confucion.bayes,
                             panel.indices.generales.bayes,
                             tabs.bayes))

# PAGINA DE REDES NEURONALES ----------------------------------------------------------------------------------------------

opciones.nn <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,execute.btn("runNn"))),
                    hr(),
                    fluidRow(column(numericInput("threshold.nn",labelInput("threshold"),
                                                 min = 0, step = 0.01, value = 0.05), width = 6),
                             column(numericInput("stepmax.nn",labelInput("stepmax"),
                                                 min = 100, step = 100, value = 5000), width = 6)),
                    fluidRow(column(sliderInput(inputId = "cant.capas.nn", min = 1, max = 10,
                                                 label = labelInput("selectCapas"), value = 2), width = 12)),
                    fluidRow(id = "capasFila",lapply(1:10, function(i) tags$span(numericInput(paste0("nn.cap.",i), NULL,
                                                                    min = 1, step = 1, value = 2),
                                                                 class = "mini-numeric-select"))))

codigo.nn <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxNn == 'tabNnModelo'",
                                    aceEditor("fieldCodeNn", mode = "r", theme = "monokai", value = "", height = "22vh", readOnly = T)),
                  conditionalPanel("input.BoxNn == 'tabNnPlot'",
                                   aceEditor("fieldCodeNnPlot", mode = "r", theme = "monokai", value = "", height = "9vh", readOnly = T)),
                   conditionalPanel("input.BoxNn == 'tabNnPred'",
                                    aceEditor("fieldCodeNnPred", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxNn == 'tabNnMC'",
                                    aceEditor("fieldCodeNnMC", mode = "r", theme = "monokai",
                                              value = "", height = "13vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxNn == 'tabNnIndex'",
                                    aceEditor("fieldCodeNnIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.nn <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(75,100), heights = c(95, 95),
                        tabs.content = list(opciones.nn, codigo.nn))

plot.nn <- tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
                    plotOutput('plot.nn', height = "55vh"))

panel.generar.nn <- tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
                              verbatimTextOutput("txtnn"))

panel.prediccion.nn <- tabPanel(title = labelInput("predm"), value = "tabNnPred",
                                 DT::dataTableOutput("nnPrediTable"))

panel.matriz.confucion.nn <- tabPanel(title = labelInput("mc"), value = "tabNnMC",
                                       plotOutput('plot.nn.mc', height = "45vh"),
                                       verbatimTextOutput("txtnnMC"))

panel.indices.generales.nn <- tabPanel(title = labelInput("indices"), value = "tabNnIndex",
                                        fluidRow(column(width = 6, gaugeOutput("nnPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("nnErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("nnIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("nnIndErrTable"))))

pagina.nn  <- tabItem(tabName = "nn",
                      tabBox(id = "BoxNn", width = NULL, height ="80%",
                             panel.generar.nn,
                             plot.nn,
                             panel.prediccion.nn,
                             panel.matriz.confucion.nn,
                             panel.indices.generales.nn,
                             tabs.nn))

# PAGINA DE XG BOOSTING ---------------------------------------------------------------------------------------------------

opciones.xgb <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,execute.btn("runXgb"))),
                     hr(),
                     fluidRow(column(selectInput(inputId = "boosterXgb", label = labelInput("selbooster"),selected = 1,
                                                 choices = c("gbtree", "gblinear", "dart")),width = 12)),
                     fluidRow(column(numericInput("maxdepthXgb", labelInput("maxdepth"), min = 1,step = 1, value = 6), width = 6),
                              column(numericInput("nroundsXgb", labelInput("selnrounds"), min = 0,step = 1, value = 50), width = 6)))

codigo.xgb <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxXgb == 'tabXgbModelo'",
                                    aceEditor("fieldCodeXgb", mode = "r", theme = "monokai", value = "", height = "19vh", readOnly = T)),
                   conditionalPanel("input.BoxXgb == 'tabXgbImp'",
                                    aceEditor("fieldCodeXgbImp", mode = "r", theme = "monokai", value = "", height = "8vh", readOnly = T)),
                   conditionalPanel("input.BoxXgb == 'tabXgbPred'",
                                    aceEditor("fieldCodeXgbPred", mode = "r", theme = "monokai",
                                              value = "", height = "17vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxXgb == 'tabXgbMC'",
                                    aceEditor("fieldCodeXgbMC", mode = "r", theme = "monokai",
                                              value = "", height = "17vh", readOnly = T, autoComplete = "enabled")),
                   conditionalPanel("input.BoxXgb == 'tabXgbIndex'",
                                    aceEditor("fieldCodeXgbIG", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.xgb <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(55,100), heights = c(70, 95),
                        tabs.content = list(opciones.xgb, codigo.xgb))

panel.generar.xgb <- tabPanel(title = labelInput("generatem"), value = "tabXgbModelo",
                              verbatimTextOutput("txtxgb"))

plot.xgb <- tabPanel(title = labelInput("varImp"), value = "tabXgbImp",
                    plotOutput('plot.xgb', height = "55vh"))

panel.prediccion.xgb <- tabPanel(title = labelInput("predm"), value = "tabXgbPred",
                                 DT::dataTableOutput("xgbPrediTable"))

panel.matriz.confucion.xgb <- tabPanel(title = labelInput("mc"), value = "tabXgbMC",
                                       plotOutput('plot.xgb.mc', height = "45vh"),
                                       verbatimTextOutput("txtxgbMC"))

panel.indices.generales.xgb <- tabPanel(title = labelInput("indices"), value = "tabXgbIndex",
                                        fluidRow(column(width = 6, gaugeOutput("xgbPrecGlob", width = "100%")),
                                                 column(width = 6, gaugeOutput("xgbErrorGlob", width = "100%"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("xgbIndPrecTable"))),
                                        fluidRow(column(width = 12, shiny::tableOutput("xgbIndErrTable"))))

pagina.xgb <- tabItem(tabName = "xgb",
                      tabBox(id = "BoxXgb", width = NULL, height ="80%",
                             panel.generar.xgb,
                             plot.xgb,
                             panel.prediccion.xgb,
                             panel.matriz.confucion.xgb,
                             panel.indices.generales.xgb,
                             tabs.xgb))

# PAGINA DE REGRESION LINEAR -------------------------------------------------------------------------------------

codigo.rl <- list(fluidRow(column(width = 11,h4(labelInput("codigo"))),
                           column(width = 1,execute.btn("runRl"))),
                  hr(),
                  conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                   aceEditor("fieldCodeRl", mode = "r", theme = "monokai", value = "", height = "22vh", readOnly = T)),
                  conditionalPanel("input.BoxRl == 'tabRlPred'",
                                   aceEditor("fieldCodeRlPred", mode = "r", theme = "monokai",
                                             value = "", height = "10vh", readOnly = T, autoComplete = "enabled")),
                  conditionalPanel("input.BoxRl == 'tabRlMC'",
                                   aceEditor("fieldCodeRlMC", mode = "r", theme = "monokai",
                                             value = "", height = "13vh", readOnly = T, autoComplete = "enabled")),
                  conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                   aceEditor("fieldCodeRlIG", mode = "r", theme = "monokai",
                                             value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.rl <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(95),
                       tabs.content = list(codigo.rl))

panel.generar.rl <- tabPanel(title = labelInput("generatem"), value = "tabRlModelo",
                             verbatimTextOutput("txtrl"))

panel.prediccion.rl <- tabPanel(title = labelInput("predm"), value = "tabRlPred",
                                DT::dataTableOutput("rlPrediTable"))

panel.matriz.confucion.rl <- tabPanel(title = labelInput("mc"), value = "tabRlMC",
                                      plotOutput('plot.rl.mc', height = "45vh"),
                                      verbatimTextOutput("txtrlMC"))

panel.indices.generales.rl <- tabPanel(title = labelInput("indices"), value = "tabRlIndex",
                                       fluidRow(column(width = 6, gaugeOutput("rlPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("rlErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rlIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rlIndErrTable"))))

pagina.rl  <- tabItem(tabName = "rl",
                      tabBox(id = "BoxRl", width = NULL, height ="80%",
                             panel.generar.rl,
                             panel.prediccion.rl,
                             panel.matriz.confucion.rl,
                             panel.indices.generales.rl,
                             tabs.rl))

# PAGINA DE RLR -----------------------------------------------------------------------------------------------------------

opciones.rlr <- list(fluidRow(column(width = 9,h4(labelInput("codigo"))),
                              column(width = 3,execute.btn("runRlr"))),
                     hr(),
                     fluidRow(column(selectInput(inputId = "alpha.rlr", label = labelInput("selectAlg"),selected = 1,
                                                 choices = list("Ridge" = 0, "Lasso" = 1)),width = 6),
                              column(br(), switchInput(inputId = "switch.scale.rlr", onStatus = "success", offStatus = "danger", value = T,
                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                     fluidRow(column(id = "colManualLanda",width = 6, numericInput("landa", labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                              column(width = 6, switchInput(inputId = "permitir.landa", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                            label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))

codigo.rlr  <- list(fluidRow(column(width = 9,h4(labelInput("codigo")))),
                    hr(),
                    conditionalPanel("input.BoxRlr == 'tabRlrModelo'",
                                     aceEditor("fieldCodeRlr", mode = "r", theme = "monokai",
                                               value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRlr == 'tabRlrLanda'",
                                     aceEditor("fieldCodeRlrLanda", mode = "r", theme = "monokai",
                                               value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                     aceEditor("fieldCodeRlrPosibLanda", mode = "r", theme = "monokai",
                                               value = "", height = "8vh", readOnly = T, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                     aceEditor("fieldCodeRlrPred", mode = "r", theme = "monokai",
                                               value = "", height = "10vh", readOnly = T, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRlr == 'tabRlrMC'",
                                     aceEditor("fieldCodeRlrMC", mode = "r", theme = "monokai",
                                               value = "", height = "17vh", readOnly = T, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                     aceEditor("fieldCodeRlrIG", mode = "r", theme = "monokai",
                                               value = "", height = "28vh", readOnly = T, autoComplete = "enabled")))

tabs.rlr  <- tabsOptions(botones = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                         tabs.content = list(opciones.rlr, codigo.rlr))

panel.generar.rlr <- tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
                              verbatimTextOutput("txtRlr"))

panel.posib.landa.rlr <- tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
                                  plotOutput('plot.rlr.posiblanda', height = "55vh"))

panel.landa.rlr <- tabPanel(title = labelInput("gcoeff"),value = "tabRlrLanda",
                            plotOutput('plot.rlr.landa', height = "55vh"))

panel.prediccion.rlr <- tabPanel(title = labelInput("predm"), value = "tabRlrPred",
                                 DT::dataTableOutput("rlrPrediTable"))

panel.matriz.confucion.rlr <- tabPanel(title = labelInput("mc"), value = "tabRlrMC",
                                       plotOutput('plot.rlr.mc', height = "45vh"),
                                       verbatimTextOutput("txtrlrMC"))

panel.indices.generales.rlr <- tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
                                       fluidRow(column(width = 6, gaugeOutput("rlrPrecGlob", width = "100%")),
                                                column(width = 6, gaugeOutput("rlrErrorGlob", width = "100%"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rlrIndPrecTable"))),
                                       fluidRow(column(width = 12, shiny::tableOutput("rlrIndErrTable"))))

pagina.rlr <- tabItem(tabName = "rlr",
                      tabBox(id = "BoxRlr", width = NULL, height ="80%",
                             panel.generar.rlr,
                             panel.posib.landa.rlr,
                             panel.landa.rlr,
                             panel.prediccion.rlr,
                             panel.matriz.confucion.rlr,
                             panel.indices.generales.rlr,
                             tabs.rlr))

# PAGINA DE COMPARACION DE MODELOS ---------------------------------------------------------------------------------------

selector.modelos <- checkboxGroupButtons("select.models", labelInput("selectMod"), c(" ---- " = "NoDisponible"),
                                         size = "sm", status = "primary",
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                          no = icon("remove", lib = "glyphicon")))

opciones.comparacion <- list(fluidRow(column(width = 10,h4(labelInput("opciones")))),
                             hr(),
                             fluidRow(column(selectInput(inputId = "roc.sel",label = labelInput("selectCat"),choices =  "", width = "100%"), width = 12)),
                             fluidRow(column(selector.modelos, width = 12)))


tabs.comparacion  <- tabsOptions(botones = list(icon("gear")), widths = c(100), heights = c(88),
                                 tabs.content = list(opciones.comparacion))

panel.comparacion.tabla <- tabPanel(title = labelInput("tablaComp"),
                                    DT::dataTableOutput("TablaComp", height="70vh"))

plot.comparacion.roc <- tabPanel(title = labelInput("rocCurva"), plotOutput('plot.roc', height = "70vh"))


pagina.comparacion <- tabItem(tabName = "comparar",
                              tabBox(id = "BoxCom", width = NULL, height ="80%",
                                     panel.comparacion.tabla,
                                     plot.comparacion.roc,
                                     tabs.comparacion))



# PAGINA DE PREDICCIONES NUEVAS ---------------------------------------------------------------------------------------

muestra.datos.pred <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                          DT::DTOutput('contentsPred'), type = 7, color = "#CBB051")

muestra.datos.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred2'), type = 7, color = "#CBB051")

muestra.datos.pred3 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred3'), type = 7, color = "#CBB051")

panel.cargar.datos.pred <- tabPanel(title = labelInput("cargarDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               fluidRow(column(width = 5,
                               checkboxInput('headerNPred', labelInput("header"), TRUE),
                               checkboxInput('rownameNPred', labelInput("Rownames"), TRUE),
                               radioButtonsTr('sepNPred', 'separador', c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                               radioButtonsTr('decNPred',"separadordec", c(',', '.'), c("coma", "punto")),
                               switchInput(inputId = "deleteNAnPred", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                               fileInput('file2', label = labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton("loadButtonNPred", labelInput("cargar"), width = "100%")),
                               column(width = 7, muestra.datos.pred)))


panel.tansformar.datos <- tabPanel(title = labelInput("transDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   fluidRow(column(width = 5,
                                                   DT::dataTableOutput('transDataPredN'),
                                                   br(),br(),
                                                   actionButton("transButtonPredN", labelInput("aplicar"), width = "100%")),
                                   column(width = 7, muestra.datos.pred2)))

panel.cargar.datos.pred2 <- tabPanel(title = labelInput("cargarNuev"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                    fluidRow(column(width = 5,
                                                    checkboxInput('headerNPred2', labelInput("header"), TRUE),
                                                    checkboxInput('rownameNPred2',  labelInput("Rownames"), TRUE),
                                                    radioButtonsTr('sep.nPred2', 'separador', c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                                                    radioButtonsTr('dec.nPred2', "separadordec", c(',', '.'), c("coma", "punto")),
                                                    switchInput(inputId = "deleteNAnPred2", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                                                label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                                                    fileInput('file3', label = labelInput("cargarchivo"), placeholder = "", buttonLabel = labelInput("subir"), width = "100%",
                                                              accept = c('text/csv', '.csv')),
                                                    actionButton("loadButtonNPred2", labelInput("cargar"), width = "100%")),
                                             column(width = 7, muestra.datos.pred3)))


opciones.knn.pred <- fluidRow(column(width = 4, br() , switchInput(inputId = "switch.scale.knn.pred", onStatus = "success", offStatus = "danger", value = T,
                                                              label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                              column(width = 4, numericInput("kmax.knn.pred", labelInput("kmax"), min = 1,step = 1, value = 7,width="100%")),
                              column(width = 4, selectInput(inputId = "kernel.knn.pred", label = labelInput("selkernel") ,selected = 1, width="100%",
                                                                      choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                                   "triweight", "cos","inv","gaussian"))))

opciones.svm.pred <- fluidRow(column(width = 6, br(), switchInput(inputId = "switch.scale.svm.pred", onStatus = "success", offStatus = "danger", value = T,
                                                           label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                         column(width = 6, selectInput(inputId = "kernel.svm.pred", label = labelInput("selkernel"), selected = "radial", width="100%",
                                                           choices =  c("linear", "polynomial", "radial", "sigmoid"))))

opciones.dt.pred <- fluidRow(column(width = 4, numericInput("minsplit.dt.pred", labelInput("minsplit"), 20, width = "100%",min = 1)),
                             column(width = 4, numericInput("maxdepth.dt.pred", labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                             column(width = 4,selectInput(inputId = "split.dt.pred", label = labelInput("splitIndex"),selected = 1,width = "100%",choices =  c("gini", "information"))))

opciones.rf.pred <- fluidRow(column(width = 6, numericInput("ntree.rf.pred", labelInput("numTree"), 20, width = "100%", min = 0)),
                             column(width = 6, numericInput("mtry.rf.pred",labelInput("numVars"),1, width = "100%", min = 1)))

opciones.boosting.pred <- list(fluidRow(column(width = 3, numericInput("iter.boosting.pred", labelInput("numTree"), 50, width = "100%",min = 1)),
                                   column(width = 3, numericInput("maxdepth.boosting.pred",labelInput("maxdepth"), 15, width = "100%",min = 1)),
                                   column(width = 3, numericInput("minsplit.boosting.pred",labelInput("minsplit"), 20, width = "100%",min = 1)),
                                   column(width = 3, selectInput(inputId = "tipo.boosting.pred", label = labelInput("selectAlg"),selected = 1, width = "100%",
                                                                 choices =  c("discrete", "real", "gentle")))))

opciones.bayes.pred <- tags$span()#vacio

opciones.xgb.pred <- fluidRow(column(numericInput("maxdepthXgb.pred", labelInput("maxdepth"), min = 1,step = 1, value = 6), width = 4),
                              column(numericInput("nroundsXgb.pred", labelInput("selnrounds"), min = 0,step = 1, value = 50), width = 4),
                              column(selectInput(inputId = "boosterXgb.pred", label = labelInput("selbooster"),selected = 1,
                                                 choices = c("gbtree", "gblinear", "dart")),width = 4))

opciones.nn.pred <-list(fluidRow(column(numericInput("threshold.nn.pred",labelInput("threshold"),
                                                     min = 0, step = 0.01, value = 0.05), width = 4),
                                 column(numericInput("stepmax.nn.pred",labelInput("stepmax"),
                                                     min = 100, step = 100, value = 5000), width = 4),
                                 column(sliderInput(inputId = "cant.capas.nn.pred", min = 1, max = 10,
                                                    label = labelInput("selectCapas"), value = 10), width = 4)),
                        fluidRow(lapply(1:10, function(i) tags$span(numericInput(paste0("nn.cap.pred.",i), NULL,
                                                                                 min = 1, step = 1, value = 2),
                                                                    class = "mini-numeric-select"))))


opciones.rl.pred <- tags$span()#vacio

opciones.rlr.pred <- list(fluidRow(column(selectInput(inputId = "alpha.rlr.pred", label = labelInput("selectAlg"),selected = 1,
                                                     choices = list("Ridge" = 0, "Lasso" = 1)),width = 6),
                                  column(br(), switchInput(inputId = "switch.scale.rlr.pred", onStatus = "success", offStatus = "danger", value = T,
                                                           label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                         fluidRow(column(id = "colManualLanda.pred",width = 6, numericInput("landa.pred", labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                                  column(width = 6, switchInput(inputId = "permitir.landa.pred", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))


opciones.modelo <- list(selectInput(inputId = "sel.predic.var.nuevos", label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                        radioGroupButtons("selectModelsPred", labelInput("selectMod"), list("<span data-id=\"knnl\"></span>" = "knn",
                                                                                            "<span data-id=\"dtl\"></span>" = "dt",
                                                                                            "<span data-id=\"rfl\"></span>" = "rf",
                                                                                            "<span data-id=\"bl\"></span>" = "ada",
                                                                                            "<span data-id=\"svml\"></span>" = "svm",
                                                                                            "Bayes" = "bayes",
                                                                                            "<span data-id=\"xgb\"></span>" = "xgb",
                                                                                            "<span data-id=\"nn\"></span>" = "nn",
                                                                                            "<span data-id=\"rl\"></span>" = "rl",
                                                                                            "<span data-id=\"rlr\"></span>" = "rlr"),
                                          size = "sm", status = "primary",individual = FALSE, justified = FALSE, selected = "knn",
                                          checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                           no = icon("remove", lib = "glyphicon")), width = "100%"))

panel.crear.modelo.pred <- tabPanel(title = labelInput("seleParModel"),solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                                    opciones.modelo,
                                    conditionalPanel(condition =  "input.selectModelsPred == 'knn'",
                                                     opciones.knn.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'dt'",
                                                     opciones.dt.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rf'",
                                                     opciones.rf.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'ada'",
                                                     opciones.boosting.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'svm'",
                                                     opciones.svm.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'bayes'",
                                                     opciones.bayes.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'nn'",
                                                     opciones.nn.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'xgb'",
                                                     opciones.xgb.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rl'",
                                                     opciones.rl.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rlr'",
                                                     opciones.rlr.pred),
                                    verbatimTextOutput("txtPredNuevos"),
                                    actionButton("PredNuevosBttnModelo", labelInput("generarM"), width  = "100%" ))

tabs.modelos  <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                       tabs.content = list(list(aceEditor("fieldPredNuevos", mode = "r", theme = "monokai", value = "", height = "20vh", readOnly = T))))

tabs.modelos2  <- tabsOptions(botones = list(icon("code")), widths = c(100), heights = c(40),
                             tabs.content = list(aceEditor("fieldCodePredPN", mode = "r", theme = "monokai",
                                                           value = "", height = "20vh", readOnly = T, autoComplete = "enabled")))

panel.prediccion.pred <- tabPanel(title = labelInput("predicnuevos"), value = "predicModelo",
                                 DT::dataTableOutput("PrediTablePN"),
                                 hr(),
                                 downloadButton("downloaDatosPred", labelInput("descargar"), style = "width:100%;"),
                                 actionButton("predecirPromidat", "preditc"))

pagina.predicciones.nuevas <- tabItem(tabName = "predNuevos",
                                      tabBox(id = "BoxModelo", width = NULL, height ="80%",
                                             panel.cargar.datos.pred,
                                             panel.tansformar.datos,
                                             panel.crear.modelo.pred,
                                             panel.cargar.datos.pred2,
                                             panel.prediccion.pred,
                                             conditionalPanel(condition =  "input.BoxModelo == 'crearModelo'", tabs.modelos),
                                             conditionalPanel(condition =  "input.BoxModelo == 'predicModelo'", tabs.modelos2)))

# PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

panel.reporte.encabezado <- column(width = 5, box(title = labelInput("reporte"), width = 12,
                                              textInput("textTitulo", value = "Sin Titulo", width = "100%", label = labelInput("titulo")),
                                              textInput("textNombre", value = "PROMiDAT", width = "100%", label = labelInput("nombre")),
                                              downloadButton("descargar", labelInput("descargar"), class = "center-button")))

panel.reporte.codigo <- column(width = 7,box(title = labelInput("codreporte"), width = 12, height = "50vh",status = "primary", solidHeader = TRUE,
                                             collapsible = TRUE, aceEditor("fieldCodeReport", mode="markdown", value='', height = "43vh", readOnly = T)))

panel.reporte.salida <- fluidRow(column(width = 12, box(title = labelInput("salida"), width = 12, height = "35vh", verbatimTextOutput("txtreport"))))


pagina.generar.reporte <- tabItem(tabName = "reporte",
                                  fluidRow(panel.reporte.encabezado ,
                                  panel.reporte.codigo),
                                  panel.reporte.salida)

# PAGINA DE INFORMACION ---------------------------------------------------------------------------------------------------

pagina.info <- tabItem(tabName = "acercaDe",
                       img(src="Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
                       infoBoxPROMiDAT(labelInput("copyright"), "PROMiDAT S.A.", icono = icon("copyright")),
                       infoBoxPROMiDAT(labelInput("info"), tags$a( href="https://www.promidat.com/", style = "color:white;",
                                                                   target = "_blank", "https://www.promidat.com"), icono = icon("info")),
                       infoBoxPROMiDAT(labelInput("version"), "1.1.3", icono = icon("file-code-o")))

# PAGINA COMPLETA ---------------------------------------------------------------------------------------------------------

shinyUI(
  dashboardPagePlus(
  title="PROMiDAT - PredictoR",
  dashboardHeaderPlus(
    title = tags$a(href="http://promidat.com", target = "_blank",
                   img(src="Logo2.png", height=55, width="100%",
                       id="imgPromidat"))),
  dashboardSidebar(mi.menu),
  dashboardBody(mi.head,
                load.page,
                tabItems(pagina.cargar.datos,
                         pagina.resumen.numerico,
                         pagina.test.normalidad,
                         pagina.distribuciones,
                         pagina.dispersion,
                         pagina.correlaciones,
                         pagina.poder,
                         pagina.knn,
                         pagina.svm,
                         pagina.dt,
                         pagina.rf,
                         pagina.boosting,
                         pagina.bayes,
                         pagina.nn,
                         pagina.xgb,
                         pagina.rl,
                         pagina.rlr,
                         pagina.comparacion,
                         pagina.predicciones.nuevas,
                         pagina.generar.reporte,
                         pagina.info) )))
