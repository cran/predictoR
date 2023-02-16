#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @keywords internal
app_server <- function( input, output, session ) {
  
  ##################################  Options  ################################
  options(shiny.maxRequestSize = 200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'), emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next"     = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first"    = shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last"     = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )
  
  onStop(function() stopApp())
  exe(paste0("library(traineR)"))
  ##################################  Variables  ##############################
  updateData <- rv(datos              = NULL, 
                   originales         = NULL, 
                   datos.tabla        = NULL, 
                   datos.prueba       = NULL, 
                   datos.aprendizaje  = NULL,
                   variable.predecir  = NULL,
                   indices            = NULL, 
                   numGrupos          = NULL, 
                   numValC            = NULL, 
                   grupos             = NULL)
  
  codedioma <- rv(idioma             = NULL,
                  code = list())
  
  updateData2 <- rv(datos              = NULL, 
                    originales         = NULL, 
                    datos.tabla        = NULL, 
                    datos.prueba       = NULL, 
                    datos.aprendizaje  = NULL,
                    variable.predecir  = NULL,
                    indices            = NULL, 
                    numGrupos          = NULL, 
                    numValC            = NULL, 
                    grupos             = NULL)
  
  newCases   <-     rv(originales        = NULL, 
                       datos.prueba      = NULL, 
                       datos.aprendizaje = NULL,
                       m.seleccionado    = NULL,
                       modelo            = NULL,
                       prediccion        = NULL,
                       variable.predecir = NULL)
  
  modelos    <-  rv(svm      = NULL,
                    knn      = NULL,
                    bayes    = NULL,
                    rl       = NULL,
                    rlr      = NULL,
                    xgb      = NULL,
                    boosting = NULL,
                    rf       = NULL,
                    nn       = NULL,
                    dt       = NULL)
  
  modelos2    <-  rv(svm      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     knn      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     bayes    = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rl       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rlr      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     xgb      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     boosting = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rf       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     nn       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     dt       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     lda      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     qda      = list(n = 0, mcs = vector(mode = "list", length = 10)))
  ###################################  Update  ################################

  #' Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  observeEvent(updateData$datos, {
    modelos2    <-  rv(svm      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       knn      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       bayes    = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       rl       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       rlr      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       xgb      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       boosting = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       rf       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       nn       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       dt       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       lda      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                       qda      = list(n = 0, mcs = vector(mode = "list", length = 10)))
  })
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg <- input$idioma
    
    keys <- names(translation)

    for (k in keys) {
      codigo <- gsub(paste0(" ", k, "\n"), paste0(" ", tr(k, idioma = lg), "\n"), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(xgboost)\n",
      "library(rpart)\n", "library(rpart.plot)\n",
      "library(glmnet)\n", "library(predictoR)\n",
      "library(echarts4r)\n", "library(loadeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  output$btn_code <- downloadHandler(
    filename = "codigo.R",
    content = function(con) {
      write(input$fieldCode, con)
    }
  )
  
  #' Enable/disable on load data
  observe({
    element <- "#sidebarItemExpanded li"
    menu.values <- c(
      "[class^=treeview]",  " a[data-value=acp]",  " a[data-value=parte1]", " a[data-value=cj]",
      " a[data-value=kmedias]", " a[data-value=reporte]")
    
    lapply(menu.values, function(i){
      if(is.null(updateData$datos) || ncol(updateData$datos) < 1) {
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
      }
    })
    
    menu.values.segment <- c(
      " a[data-value=poderPred]", " a[data-value=parte2]", " a[data-value=knn]")
    
    lapply(menu.values.segment, function(i){
      if(is.null(updateData$datos.prueba) || ncol(updateData$datos.prueba) < 1) {
        shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
        shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
        addClass(class = "disabled", selector = paste0(element, i))
      } else {
        removeClass(class = "disabled", selector = paste0(element, i))
        shinyjs::enable(selector = 'a[href^="#shiny-tab-parte2"]')
        shinyjs::enable(selector = 'a[href^="#shiny-tab-comparar"]')
        
      }
      if(is.null(updateData$grupos) || (is.null(updateData$numValC) && updateData$numValC <= 1)) {
        shinyjs::disable(selector = 'a[href^="#shiny-tab-calibracion"]')
        shinyjs::disable(selector = 'a[href^="#shiny-tab-cv_cv"]')
      } else {
        shinyjs::enable(selector = 'a[href^="#shiny-tab-calibracion"]')
        shinyjs::enable(selector = 'a[href^="#shiny-tab-cv_cv"]')
        shinyjs::enable(selector = 'a[data-value=poderPred]')
      }
      
    })
  })
  
  
  ###################################  Modules  ###############################
  #Carga de Datos
  loadeR::mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, codedioma, "predictoR")
  loadeR::mod_carga_datos_server("carga_datos_ui_2", updateData2, NULL, codedioma, "discoveR")
  #Estadísticas Básicas
  loadeR::mod_r_numerico_server("r_numerico_ui_1",         updateData, codedioma)
  loadeR::mod_normal_server("normal_ui_1",                 updateData, codedioma)
  loadeR::mod_dispersion_server("dispersion_ui_1",         updateData, codedioma)
  loadeR::mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  loadeR::mod_correlacion_server("correlacion_ui_1",       updateData, codedioma)
  mod_poder_pred_server("poder_pred_ui_1",                 updateData, codedioma)
  
  #Aprendizaje Supervisado
  callModule(mod_knn_server,            "knn_ui_1",            updateData, modelos, codedioma, modelos2)
  callModule(mod_svm_server,            "svm_ui_1",            updateData, modelos, codedioma, modelos2)
  callModule(mod_d_tree_server,         "d_tree_ui_1",         updateData, modelos, codedioma, modelos2)
  callModule(mod_r_forest_server,       "r_forest_ui_1",       updateData, modelos, codedioma, modelos2)
  callModule(mod_xgboosting_server,     "xgboosting_ui_1",     updateData, modelos, codedioma, modelos2)
  callModule(mod_boosting_server,       "boosting_ui_1",       updateData, modelos, codedioma, modelos2)
  callModule(mod_bayes_server,          "bayes_ui_1",          updateData, modelos, codedioma, modelos2)
  callModule(mod_neural_net_server,     "neural_net_ui_1",     updateData, modelos, codedioma, modelos2)
  callModule(mod_l_regression_server,   "l_regression_ui_1",   updateData, modelos, codedioma, modelos2)
  callModule(mod_penalized_l_r_server,  "penalized_l_r_ui_1",  updateData, modelos, codedioma, modelos2)
  callModule(mod_lda_server,            "lda_ui_1",            updateData, modelos, codedioma, modelos2)
  callModule(mod_qda_server,            "qda_ui_1",            updateData, modelos, codedioma, modelos2)
  
  #Comparación de Modelos
  callModule(mod_comparacion_server,    "comparacion_ui_1",    updateData, modelos, codedioma, modelos2)
  callModule(mod_varerr_server,         "varerr_ui_1",         updateData, modelos, codedioma, modelos2)
  
  
  #Validación Cruzada
  callModule(mod_cv_knn_server,          "cv_knn_ui_1",           updateData, codedioma)
  callModule(mod_cv_svm_server,          "cv_svm_ui_1",           updateData, codedioma)
  callModule(mod_cv_dt_server,           "cv_dt_ui_1",            updateData, codedioma)
  callModule(mod_cv_rf_server,           "cv_rf_ui_1",            updateData, codedioma)
  callModule(mod_cv_xgb_server,          "cv_xgb_ui_1",           updateData, codedioma)
  callModule(mod_cv_rlr_server,          "cv_rlr_ui_1",           updateData, codedioma)
  callModule(mod_cv_bayes_server,        "cv_bayes_ui_1",         updateData, codedioma)
  callModule(mod_cv_rl_server,           "cv_rl_ui_1",            updateData, codedioma)
  callModule(mod_cv_boost_server,        "cv_boost_ui_1",         updateData, codedioma)
  callModule(mod_cv_lda_server,          "cv_lda_ui_1",           updateData, codedioma)
  callModule(mod_cv_qda_server,          "cv_qda_ui_1",           updateData, codedioma)
  callModule(mod_cross_validation_server,"cross_validation_ui_1", updateData, codedioma)

  #Predicción de Individuos Nuevos
  callModule(mod_ind_nuevos_server,     "ind_nuevos_ui_1",  newCases, updateData2, codedioma)
  
}
