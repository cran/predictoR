#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import rlang
#' @import traineR 
#' @import shinyAce
#' @import echarts4r
#' @import htmltools
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @importFrom xtable xtable
#' @importFrom rpart.plot prp
#' @importFrom rpart path.rpart
#' @importFrom glmnet cv.glmnet
#' @importFrom DT tableHeader formatStyle
#' @importFrom utils read.table write.csv head
#' @importFrom grDevices adjustcolor hcl rainbow 
#' @importFrom xgboost xgb.importance xgb.plot.importance
#' @importFrom shinyjs useShinyjs show hide addClass removeClass
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem tabBox
#' @importFrom graphics abline legend lines pairs par points polygon rect smoothScatter strwidth text 
#' @importFrom stats cor cutree hclust median na.omit as.formula loess model.frame model.matrix predict predict.lm qt symnum cov2cor pt
#' @keywords internal
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shinydashboardPlus::dashboardPage(
      title = "PROMiDAT",
      shinydashboardPlus::dashboardHeader(
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        )), 
        controlbarIcon = icon("cogs")
      ),
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style = "padding-top:10px;"),
          menuItem(labelInput("data"), icon = icon("database"),
                                                          tabName = "cargar"),
          menuItem(labelInput("basico"), tabName = "parte1",
                   icon = icon("th-list"),
                   menuSubItem(labelInput("resumen"), "resumen",
                               icon = icon("sort-numeric-down")),
                   menuSubItem(labelInput("normalidad"), "normalidad",
                               icon = icon("chart-bar")),
                   menuSubItem(labelInput("dispersion"), "dispersion",
                               icon = icon("chart-line")),
                   menuSubItem(labelInput("distribucion"), "distribucion",
                               icon = icon("chart-area")),
                   menuSubItem(labelInput("correlacion"), "correlacion",
                               icon = icon("table"))                   ,
                   menuSubItem(labelInput("poderpred"), "poderPred",
                               icon = icon("rocket"))
          ),
          menuItem(labelInput("aprendizaje"), tabName = "parte2", 
                  icon = icon("th-list"),
                  menuSubItem(labelInput("knnl"),tabName = "knn",
                              icon = icon("dot-circle")),
                  menuSubItem(labelInput("svml"),tabName = "svm",
                              icon = icon("vector-square")),
                  menuSubItem(labelInput("dtl"),tabName = "dt",
                              icon = icon("tree")),                                         
                  menuSubItem(labelInput("rfl"),tabName = "rf",
                              icon = icon("sitemap")),
                  menuSubItem(labelInput("xgb"),tabName = "xgb",
                              icon = icon("project-diagram")),
                  menuSubItem(labelInput("bl"),tabName = "boosting",
                              icon = icon("superscript")),
                  menuSubItem("Bayes",tabName = "bayes",
                              icon = icon("dice")),
                  menuSubItem(labelInput("nn"),tabName = "nn",
                              icon = icon("brain")),
                  menuSubItem(labelInput("rl"),tabName = "rl",
                              icon = icon("chart-line")),
                  menuSubItem(labelInput("rlr"),tabName = "rlr",
                              icon = icon("wave-square"))),
          menuItem(labelInput("comparacion"), tabName = "comparar", 
                   icon = icon("eye")),
          menuItem(labelInput("predicnuevos"), tabName = "predNuevos", 
                   icon = icon("table")),
          menuItem(labelInput("acercade"), tabName = "acercaDe",
                   icon = icon("info")),
          hr(),
          menu.idioma(),
          tags$div(style = "display:none;",
                   sliderInput(inputId = "aux", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10),
                   radioSwitch("deleteNAaux", "eliminanaaux", c("eliminarai", "impsutar")),
                   
                   colourpicker::colourInput(
                     "auxColor", NULL, value = "red", allowTransparent = T)
          )
        )
      ),
      dashboardBody(

        tabItems(

          # Carga de Datos
          tabItem(tabName = "cargar",  
                  mod_carga_datos_ui("carga_datos_ui_1")),
          # Resumen Numérico
          tabItem(tabName = "resumen", 
                  mod_r_numerico_ui("r_numerico_ui_1")),
          
          # Test de Normalidad
          tabItem(tabName = "normalidad", 
                  mod_normal_ui("normal_ui_1")),
          
          # Dispersión
          tabItem(tabName = "dispersion",
                  mod_dispersion_ui("dispersion_ui_1")),
          
          # Distribuciones
          tabItem(tabName = "distribucion", 
                  mod_distribuciones_ui("distribuciones_ui_1")),
          
          # Correlaciones
          tabItem(tabName = "correlacion", 
                  mod_correlacion_ui("correlacion_ui_1")),

          # Poder Predictivo
          tabItem(tabName = "poderPred",
                  mod_poder_pred_ui("poder_pred_ui_1")),

          # K Vecinos
          tabItem(tabName = "knn",
                  mod_knn_ui("knn_ui_1")),

          # Support Vector Machines
          tabItem(tabName = "svm",
                  mod_svm_ui("svm_ui_1")),

          # Decision Trees
          tabItem(tabName = "dt",
                  mod_d_tree_ui("d_tree_ui_1")),

          # Random Forest
          tabItem(tabName = "rf",
                  mod_r_forest_ui("r_forest_ui_1")),

          # XGBoosting
          tabItem(tabName = "xgb",
                  mod_xgboosting_ui("xgboosting_ui_1")),

          # Potenciacion
          tabItem(tabName = "boosting",
                  mod_boosting_ui("boosting_ui_1")),

          # Bayes
          tabItem(tabName = "bayes",
                  mod_bayes_ui("bayes_ui_1")),

          # Neural Net
          tabItem(tabName = "nn",
                  mod_neural_net_ui("neural_net_ui_1")),

          # Logistic Regression
          tabItem(tabName = "rl",
                  mod_l_regression_ui("l_regression_ui_1")),

          # Penalized Logistic Regression
          tabItem(tabName = "rlr",
                  mod_penalized_l_r_ui("penalized_l_r_ui_1")),

          # Comparación de Modelos
          tabItem(tabName = "comparar",
                  mod_comparacion_ui("comparacion_ui_1")),

          # Predicción Individuos Nuevos
          tabItem(tabName = "predNuevos",
                  mod_ind_nuevos_ui("ind_nuevos_ui_1")),

          # Acerca De
          tabItem(tabName = "acercaDe", 
                  mod_acercade_ui("acercade_ui_1"))
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'predictoR'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

