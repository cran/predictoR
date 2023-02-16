#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import rlang
#' @import traineR 
#' @import loadeR 
#' @import shinyAce
#' @import echarts4r
#' @import htmltools
#' @import shinycustomloader
#' @import shinydashboardPlus
#' @importFrom xtable xtable
#' @importFrom rpart.plot prp
#' @importFrom DT tableHeader
#' @importFrom glmnet cv.glmnet
#' @importFrom utils read.table write.csv head
#' @importFrom grDevices adjustcolor hcl rainbow dev.cur
#' @importFrom xgboost xgb.importance xgb.plot.importance
#' @importFrom shinyjs useShinyjs show hide addClass removeClass runjs 
#' @importFrom stats cor cutree hclust median na.omit as.formula loess model.frame 
#' @importFrom stats pnorm qnorm sd lm qt symnum cov2cor pt model.matrix predict predict.lm density
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem tabBox
#' @importFrom graphics abline legend lines pairs par points polygon rect smoothScatter strwidth text hist identify
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
        )), controlbarIcon = icon("gears")
      ),
      dashboardSidebar(
        sidebarMenu(
          id = "principal",
          tags$div(style = "padding-top:10px;"),
          menuItem(labelInput("data"), icon = icon("database"),
                   tabName = "cargar"),
          menuItem(labelInput("basico"), tabName = "parte1",
                   icon = icon("square-poll-vertical"),
                   menuSubItem(labelInput("resumen"), "resumen",
                               icon = icon("sort-numeric-down")),
                   menuSubItem(labelInput("normalidad"), "normalidad",
                               icon = icon("chart-bar")),
                   menuSubItem(labelInput("dispersion"), "dispersion",
                               icon = icon("chart-line")),
                   menuSubItem(labelInput("distribucion"), "distribucion",
                               icon = icon("chart-area")),
                   menuSubItem(labelInput("correlacion"), "correlacion",
                               icon = icon("table")),
                   menuSubItem(labelInput("poderpred"), "poderPred",
                               icon = icon("rocket"))
          ),
          menuItem(labelInput("tt"), tabName = "parte2", 
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
                               icon = icon("rocket")),
                   menuSubItem("Bayes",tabName = "bayes",
                               icon = icon("dice")),
                   menuSubItem(labelInput("nn"),tabName = "nn",
                               icon = icon("brain")),
                   menuSubItem(labelInput("rl"),tabName = "rl",
                               icon = icon("chart-line")),
                   menuSubItem(labelInput("rlr"),tabName = "rlr",
                               icon = icon("wave-square")),
                   menuSubItem(labelInput("lda"),tabName = "lda",
                               icon = icon("chart-gantt")),
                   menuSubItem(labelInput("qda"),tabName = "qda",
                               icon = icon("superscript")),
                   menuSubItem(labelInput("comparacion"), tabName = "comparar", 
                               icon = icon("balance-scale")),
                   menuSubItem(labelInput("varError"), tabName = "varerr", 
                               icon = icon("triangle-exclamation"))),
          menuItem(labelInput("calibracion"), tabName = "calibracion",#server, award battle-net brain bug buffer check-circle chart-line chart-bar
                   icon = icon("gears"),
                   menuSubItem(labelInput("knnl"),tabName = "cv_knn",
                               icon = icon("dot-circle")),
                   menuSubItem(labelInput("svml"),tabName = "cv_svm",
                               icon = icon("vector-square")),
                   menuSubItem(labelInput("dtl"),tabName = "cv_dt",
                               icon = icon("tree")),                                         
                   menuSubItem(labelInput("rfl"),tabName = "cv_rf",
                               icon = icon("sitemap")),
                   menuSubItem(labelInput("xgb"),tabName = "cv_xgb",
                               icon = icon("project-diagram")),
                   menuSubItem("Bayes",tabName = "cv_bayes",
                               icon = icon("dice")),
                   menuSubItem(labelInput("bl"),tabName = "cv_boosting",
                               icon = icon("rocket")),
                   menuSubItem(labelInput("rl"),tabName = "cv_rl",
                               icon = icon("chart-line")),
                   menuSubItem(labelInput("rlr"),tabName = "cv_rlr",
                               icon = icon("wave-square")),
                   menuSubItem(labelInput("lda"),tabName = "cv_lda",
                               icon = icon("chart-gantt")),
                   menuSubItem(labelInput("qda"),tabName = "cv_qda",
                               icon = icon("superscript"))
                   ),
          menuItem(labelInput("seleModel"), tabName = "cv_cv", 
                   icon = icon("laptop-code")),
          menuItem(labelInput("predicnuevos"), tabName = "predNuevos", 
                   icon = icon("table")),
          menuItem(labelInput("acercade"), tabName = "acercaDe",
                   icon = icon("circle-info")),
          hr(),
          menu.idioma(),
          hr(), 
          img(src = "img/predictoR.png", style = "margin-left: auto;margin-right: auto;display: block;width: 80%;"),
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
                  loadeR::mod_carga_datos_ui("carga_datos_ui_1", labelInput("data"))),
          # Resumen Numérico
          tabItem(tabName = "resumen", 
                  loadeR::mod_r_numerico_ui("r_numerico_ui_1")),
          
          # Test de Normalidad
          tabItem(tabName = "normalidad", 
                  loadeR::mod_normal_ui("normal_ui_1")),
          
          # Dispersión
          tabItem(tabName = "dispersion",
                  loadeR::mod_dispersion_ui("dispersion_ui_1")),
          
          # Distribuciones
          tabItem(tabName = "distribucion", 
                  loadeR::mod_distribuciones_ui("distribuciones_ui_1")),
          
          # Correlaciones
          tabItem(tabName = "correlacion", 
                  loadeR::mod_correlacion_ui("correlacion_ui_1")),
          
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
          
          # Linear Discriminant Analysis
          tabItem(tabName = "lda", 
                  mod_lda_ui("lda_ui_1")),
          
          # Quadratic Discriminant Analysis
          tabItem(tabName = "qda", 
                  mod_qda_ui("qda_ui_1")),
          
          # Comparación de Modelos
          tabItem(tabName = "comparar", 
                  mod_comparacion_ui("comparacion_ui_1")),  
          
          # Comparación de Modelos
          tabItem(tabName = "varerr", 
                  mod_varerr_ui("varerr_ui_1")),   
          
          
          ############### Validación Cruzada ############### 
          tabItem(tabName = "cv_knn", 
                  mod_cv_knn_ui("cv_knn_ui_1")),
          
          tabItem(tabName = "cv_svm", 
                  mod_cv_svm_ui("cv_svm_ui_1")),
          
          tabItem(tabName = "cv_dt", 
                  mod_cv_dt_ui("cv_dt_ui_1")),
          
          tabItem(tabName = "cv_rf", 
                  mod_cv_rf_ui("cv_rf_ui_1")),
          
          tabItem(tabName = "cv_xgb", 
                  mod_cv_xgb_ui("cv_xgb_ui_1")),
          
          tabItem(tabName = "cv_boosting", 
                  mod_cv_boost_ui("cv_boost_ui_1")),
          
          tabItem(tabName = "cv_rlr", 
                  mod_cv_rlr_ui("cv_rlr_ui_1")),
          
          tabItem(tabName = "cv_bayes", 
                  mod_cv_bayes_ui("cv_bayes_ui_1")),
          
          tabItem(tabName = "cv_rl", 
                  mod_cv_rl_ui("cv_rl_ui_1")),
          
          tabItem(tabName = "cv_lda", 
                  mod_cv_lda_ui("cv_lda_ui_1")),
          
          tabItem(tabName = "cv_qda", 
                  mod_cv_qda_ui("cv_qda_ui_1")),
          
          tabItem(tabName = "cv_cv", 
                  mod_cross_validation_ui("cross_validation_ui_1")),
          
          # Predicción Individuos Nuevos
          tabItem(tabName = "predNuevos", 
                  mod_ind_nuevos_ui("ind_nuevos_ui_1")), 
          
          # Acerca De
          tabItem(tabName = "acercaDe", 
                  mod_acercade_ui("acercade_ui_1"))
        )
      ),
      dashboardControlbar(
        width = 500,
        div(
          style = "margin-right: 15px; margin-left: 15px;",
          h3(labelInput('code')), hr(),
          codigo.monokai("fieldCode", height = "70vh"),
          downloadButton("btn_code", NULL, style = "width: 100%;")
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
  
  jsCode <- '
  get_inputs = function() {
  var rowname = $("#carga_datos_ui_2-rowname")[0].checked;
  var header = $("#carga_datos_ui_2-header")[0].checked;
  var sep = $("input[name=\'carga_datos_ui_2-sep\']:checked").val();
  var dec = $("input[name=\'carga_datos_ui_2-dec\']:checked").val();
  var nas = $("input[name=\'carga_datos_ui_2-deleteNA\']:checked").val();

  Shiny.setInputValue("ind_nuevos_ui_1-jsrowname", rowname);
  Shiny.setInputValue("ind_nuevos_ui_1-jsheader", header);
  Shiny.setInputValue("ind_nuevos_ui_1-jssep", sep);
  Shiny.setInputValue("ind_nuevos_ui_1-jsdec", dec);
  Shiny.setInputValue("ind_nuevos_ui_1-jsnas", nas);
  }
  
  get_inputs_xlsx = function() {
  var rowname = $("#carga_datos_ui_2-rowname_xlsx")[0].checked;
  var header = $("#carga_datos_ui_2-header_xlsx")[0].checked;
  var num_hoja = $("#carga_datos_ui_2-num_hoja").val();
  var fila_inicio = $("#carga_datos_ui_2-fila_inicio").val();
  var col_inicio = $("#carga_datos_ui_2-col_inicio").val();
  var fila_final = $("#carga_datos_ui_2-fila_final").val();
  var col_final = $("#carga_datos_ui_2-col_final").val();
  var deleteNA_xlsx = $("input[name=\'carga_datos_ui_2-deleteNA_xlsx\']:checked").val();

  Shiny.setInputValue("ind_nuevos_ui_1-jsrowname_xlsx", rowname);
  Shiny.setInputValue("ind_nuevos_ui_1-jsheader_xlsx", header);
  Shiny.setInputValue("ind_nuevos_ui_1-jsnum_hoja", num_hoja);
  Shiny.setInputValue("ind_nuevos_ui_1-jsfila_inicio", fila_inicio);
  Shiny.setInputValue("ind_nuevos_ui_1-jscol_inicio", col_inicio);
  Shiny.setInputValue("ind_nuevos_ui_1-jsfila_final", fila_final);
  Shiny.setInputValue("ind_nuevos_ui_1-jscol_final", col_final);
  Shiny.setInputValue("ind_nuevos_ui_1-jsdeleteNA_xlsx", deleteNA_xlsx);
}
  
  get_file = function() {
  $("#carga_datos_ui_2-run_data").on("click", function(){
        var file_type = $("#carga_datos_ui_2-file_type").find(".active")[0].firstElementChild.getAttribute("data-value")
        Shiny.setInputValue("ind_nuevos_ui_1-jsfile_type", file_type);
        });
}
  '
add_resource_path('www', app_sys('app/www'))
add_resource_path('img', app_sys('app/img'))
add_resource_path('lang', app_sys('app/lang'))

tags$head(
  favicon(),
  bundle_resources(
    path = app_sys('app/www'),
    app_title = 'predictoR'
  ),
  shinyjs::useShinyjs(),
  tags$script(HTML(jsCode))
  
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert() 
)
}

