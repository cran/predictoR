#' cross_validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cross_validation_ui <- function(id){
  ns <- NS(id)
  title_comp <- list(conditionalPanel("input['cross_validation_ui_1-BoxCV'] == 'tabcvcvIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                               choices =  "", width = "100%")))))
  
  opc_knn <- list(div(col_4(numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7)),
                      col_4(selectInput(inputId = ns("kernel.knn.pred"), label = labelInput("selkernel"), selected = 1,
                                        choices = c("optimal", "rectangular", "triangular", "epanechnikov", 
                                                    "biweight", "triweight", "cos","inv","gaussian"))),
                      col_4(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no") ))),
                  div(col_6(numericInput(ns("cvknnl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvknnl_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_svm <- list(div(col_6(radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"))),
                      col_6(selectInput(inputId = ns("kernel.svm.pred"), label = labelInput("selkernel"),selected = "radial",
                                 choices = c("linear", "polynomial", "radial", "sigmoid")))),
                  div(col_6(numericInput(ns("cvsvml_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvsvml_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_rf  <- list(div(col_4(numericInput(ns("ntree.rf.pred"), labelInput("numTree"), 20, width = "100%", min = 0)),
                      col_4(numericInput(ns("mtry.rf.pred"),  labelInput("numVars"),1, width = "100%", min = 1)),
                      col_4(selectInput(inputId = ns("split.rf.pred"), label = labelInput("splitIndex"),selected = 1,
                                        choices =  list("gini" = "gini", "Entropia" = "information")))),
                  div(col_6(numericInput(ns("cvrfl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvrfl_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_dt  <- list(div(col_4(numericInput(ns("minsplit.dt.pred"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                      col_4(numericInput(ns("maxdepth.dt.pred"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                      col_4(selectInput(inputId = ns("split.dt.pred"), label = labelInput("splitIndex"),selected = 1,
                                             choices =  list("gini" = "gini", "Entropia" = "information")))),
                  div(col_6(numericInput(ns("cvdtl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvdtl_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  opc_bayes <- list(div(col_6(numericInput(ns("cvBayes_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                        col_6(selectInput(ns("cvBayes_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_potenciacion <- list(div(col_6(numericInput(ns("iter.boosting.pred"), labelInput("numTree"), 20, width = "100%",min = 1)),
                               col_6(numericInput(ns("maxdepth.boosting.pred"),labelInput("maxdepth"), 15, width = "100%",min = 1)),
                               col_6(numericInput(ns("minsplit.boosting.pred"),labelInput("minsplit"), 20, width = "100%",min = 1)),
                               col_6(selectInput(inputId = ns("coeflearn"), label = labelInput("selkernel"), selected = 1,
                                                      choices = c("Breiman", "Freund", "Zhu")))),
                           div(col_6(numericInput(ns("cvbl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                               col_6(selectInput(ns("cvbl_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  opc_rl  <- list(div(col_6(numericInput(ns("cvrl_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvrl_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_lda  <- list(div(col_6(numericInput(ns("cvlda_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                       col_6(selectInput(ns("cvlda_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_qda  <- list(div(col_6(numericInput(ns("cvqda_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                       col_6(selectInput(ns("cvqda_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))

  opc_rlr <- list(div(col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"))),
                      col_6(selectInput(inputId = ns("alpha.rlr.pred"),  label = labelInput("selectAlg"),selected = 1,
                                        choices = list("Ridge" = 0, "Lasso" = 1)))),
                  div(col_6(numericInput(ns("cvrlr_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvrlr_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_xgb <- list(div(col_4(numericInput(ns("maxdepthXgb"), labelInput("maxdepth"),  min = 1,  step = 1, value = 6)),
                      col_4(selectInput(inputId = ns("boosterXgb.pred"), label = labelInput("selbooster"), selected = 1,
                                        choices = c("gbtree", "gblinear", "dart"))),
                      col_4(numericInput(ns("nroundsXgb"),  labelInput("selnrounds"), min = 0, step = 1, value = 50 ))),
                  div(col_6(numericInput(ns("cvxgb_step"), labelInput("probC"), value = 0.5, width = "100%", min = 0, max = 1, step = 0.1)),
                      col_6(selectInput(ns("cvxgb_cat"),   choices = "", label =  labelInput("selectCat"), width = "100%"))))
  
  opc_nn <- list(div(col_4(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                             min = 0,   step = 0.01, value = 0.05)),
                          col_4(numericInput(ns("stepmax_nn"),labelInput("stepmax"),
                                             min = 100, step = 100,  value = 10000)),
                          col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                            label = labelInput("selectCapas"), value = 3))),
                 div(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                          col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 3),
                          class = "mini-numeric-select")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxCV"), 
      tabPanel(title = p(labelInput("seleModel"),class = "wrapper-tag"), value = "tabCVsvmSModelo",
               div(
                 col_12(selectInput(inputId = ns("predic_var"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"))               ),
               div(
                 col_12(checkboxGroupInput(inputId = ns("sel_models"), label = labelInput("seleModel"), choices =    c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb", "rl", "rlr", "lda", "qda"), width = "100%"))
               ),br(),br()),
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               div(
                 col_12(selectInput(inputId = ns("sel_methods"), label = labelInput("selectMod"),
                                   choices =  "", width = "100%"))
               )
               , hr(style = "border-top: 2px solid #cccccc;" ),
               conditionalPanel(condition =  "input.sel_methods == 'knnl'",
                                opc_knn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'svml'",
                                opc_svm, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'dtl'",
                                opc_dt, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rfl'",
                                opc_rf, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'xgb'",
                                opc_xgb, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'bl'",
                                opc_potenciacion, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'Bayes'",
                                opc_bayes, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'nn'",
                                opc_nn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rl'",
                                opc_rl, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rlr'",
                                opc_rlr, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'lda'",
                                opc_lda, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'qda'",
                                opc_qda, ns = ns),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv"), labelInput("generarT"), width  = "100%" ),br(),br(),
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txt_cv")), 
                                                      type = "html", loader = "loader4")),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvcvIndices3",
               div(col_8(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_cv_glob"), width = "100%", height = "80vh")),
                   col_6(echarts4rOutput(ns("e_cv_error"), width = "100%", height = "80vh")))),
      tabPanel(title = p(labelInput("indicesCat"),class = "wrapper-tag"), value = "tabcvcvIndicesCat",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectCat"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_6(echarts4rOutput(ns("e_cv_category"), width = "100%", height = "80vh")),
                   col_6(echarts4rOutput(ns("e_cv_category_err"), width = "100%", height = "80vh")))),
      tabPanel(title = p(labelInput("tablaComp"),class = "wrapper-tag"),
               withLoader(DT::dataTableOutput(ns("TablaComp"), height="80vh"), 
                          type = "html", loader = "loader4"))
    )
 
  )
}


#' cross_validation Server Functions
#'
#' @noRd 
mod_cross_validation_server <- function(input, output, session, updateData, codedioma){
    ns <- session$ns
    
    M <- rv(MCs.cv = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
    
    # Cuándo se seleccionan modelos
    observeEvent(input$sel_models, {
      nombres        <- input$sel_models
      names(nombres) <- tr(nombres,codedioma$idioma)
      updateSelectInput(session, "sel_methods", choices = nombres, selected = nombres[1]) # Actualiza el select para las opciones de cada modelo
    })
    
    # Cuando cambia el idioma
    observeEvent(codedioma$idioma, {
      # Actualiza los nombres de los modelos
      nombres <- list("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb" , "rl", "rlr", "lda", "qda")
      names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "Bayes", "xgb" , "rl", "rlr", "lda", "qda"),codedioma$idioma)
      # Obtiene los modelos seleccionados anteriormente
      modelos   <- input$sel_models
      # Obtiene eitquetas de los graficos comparativos
      precision <- list(0, 1)
      names(precision) <- tr(c("errG", "precG"),codedioma$idioma)
      nombres_p <- list( "lineas", "barras","error")
      names(nombres_p) <- tr(c("grafLineas", "grafBarras",  "grafError"),codedioma$idioma)
      
      # Actualiza los valores 
      updateSelectInput(session, "plot_type", choices = nombres_p, selected = "lineas")
      updateSelectInput(session, "plot_type_p", choices = nombres_p, selected = "lineas")
      updateCheckboxGroupInput(session, "sel_models", choices = nombres, selected = modelos)
    })
    
    # Cuándo cambian los datos y variable a predecir
    observeEvent(c(updateData$datos, updateData$variable.predecir), {
      datos    <- updateData$datos
      variable <- updateData$variable.predecir
      # Reiniciamos valores por defecto
      M$MCs.cv  <- NULL
      M$grafico <- NULL
      M$global  <- NULL
      M$categories <- NULL
      M$times      <- 0
      defaul_param_values()
      if(!is.null(datos)){
        choices      <- as.character(unique(datos[, variable])) # Actualiza categorías de la variable a predecir
        updateTextInput(session, "txt_cv", value = ' ' ) # Actualiza txt de CV
        updateSelectInput(session, "cv.cat.sel", choices = choices, selected = choices[1]) # Actualiza categoría para los gráficos
        updateSelectInput(session, "predic_var", choices = rev(colnames.empty(var.categoricas(updateData$datos)))) # Variables categóricas para seleccionar variable a predecir
        actualizar.prob.corte(choices)
      }
      
      output$txt_cv <- renderPrint({
        return(invisible(''))
      })
    })
    
    # Cuándo ejecuta el botón de CV
    observeEvent(input$btn_cv, {
      output$txt_cv <- renderPrint({
        tryCatch({
          cant.vc   <- isolate(updateData$numValC) # Obtiene cantidad de validaciones a realizar
          datos     <- isolate(updateData$datos) # Obtiene los datos
          numGrupos <- isolate(updateData$numGrupos) # Obtiene la cantidad de grupos
          grupos    <- isolate(updateData$grupos) # Obtiene los grupos de cada validación
          variable  <- isolate(updateData$variable.predecir) # Variable a predecir
          var_      <- as.formula(paste0(variable, "~."))
          category  <- isolate(levels(updateData$datos[,variable])) # Categorías de la variable a predecir
          dim_v     <- isolate(length(category)) # Cantidad de categorías (para generar las matrices de confusión)
          params    <- listar_parametros() # Obtiene los parámetros seleccionados para cada modelo
          models    <- isolate(input$sel_models) # Modelos seleccionados para validación cruzada
          nombres   <- vector(mode = "character", length = length(models)) # Inicializa vector para almacenar nombres de los modelos
          MCs.cv    <- vector(mode = "list") # Lista de listas que va a guardar todas las MCs
          if(length(category) == 2)# Si aplica obtiene la probabilidad de corte seleccionada para cada modelo
            cortes <- parametros.prob.c()
            
          if(length(models)<1){
            if(M$times != 0)
              showNotification("Debe seleccionar al menos un model")
          }

          for (model in 1:length(models)){
            # Llena la lista de listas de MCs con los nombres de cada modelo
            MCs.cv[[paste0("MCs.",models[model])]] <- vector(mode = "list", length = cant.vc)
            # Guarda los nombres para las matrices individuales
            nombres[model] <- paste0("MC.",models[model])
          }
          
          # Ejecuta Validación Cruzada
          for (i in 1:cant.vc){
            # Lista de Matrices, se identifican con el nombre del modelo
            MC.cv <- vector(mode = "list", length = length(models))
            names(MC.cv) <- nombres
            # Crea la matriz que almacena la MC de confusión
            # Toma en cuenta las dimensiones de la variable a predecir con dim_v
            for (model in 1:length(models)){
              MC.cv[[model]] <- matrix(rep(0, dim_v * dim_v), nrow = dim_v)
            }
            
            for (k in 1:numGrupos){
              # Obtiene los grupos de cada validación
              muestra   <- grupos[[i]][[k]]
              ttraining <- datos[-muestra, ]
              ttesting  <- datos[muestra, ]
              
              # Recorre los modelos seleccionados
              for (j in 1:length(models)){
                # Valida el modelo seleccionado y lo genera
                modelo      <- switch (models[j],
                                       "knnl"  = {
                                                 train.knn(var_, 
                                                           data  = ttraining, 
                                                           scale = as.logical(params$scal_kn), 
                                                           kernel = params$kernel_kn, 
                                                           kmax   = params$k_kn)}, 
                                       "svml"  = {
                                                 train.svm(var_, 
                                                           data = ttraining, 
                                                           scale  = as.logical(params$scal_svm), 
                                                           kernel = params$kernel_svm)}, 
                                       "dtl"   = {
                                                 train.rpart(var_, 
                                                             data    = ttraining,
                                                             control = rpart.control(minsplit = params$minsplit_dt, 
                                                                                     maxdepth = params$maxdepth_dt),
                                                             parms   = list(split = params$tipo_dt))}, 
                                       "xgb"   = {
                                                 train.xgboost(var_, 
                                                               data      = ttraining, 
                                                               booster   = params$tipo_xgb, 
                                                               max_depth = params$maxdepth_xgb, 
                                                               nrounds   = params$n.rounds, 
                                                               verbose   = 0)}, 
                                       "rfl"   = {
                                                 train.randomForest(var_, 
                                                                    data  = ttraining, 
                                                                    mtry  = params$mtry, 
                                                                    ntree = params$ntree, 
                                                                    importance = TRUE,
                                                                    parms   = list(split = params$tipo_rf))},
                                       "bl"    = {
                                                 train.adabag(var_, 
                                                              data      = ttraining, 
                                                              coeflearn = params$coeflearn_b, 
                                                              mfinal    = 100,
                                                              control   = rpart.control(minsplit = params$minsplit_b, 
                                                                                        maxdepth = params$maxdepth_b))}, 
                                       "Bayes" = {
                                                 train.bayes(var_, 
                                                             data = ttraining)}, 
                                       "rl"    = {
                                                 train.glm(var_, 
                                                           data = ttraining)}, 
                                       "rlr"   = {
                                                 train.glmnet(var_, 
                                                              data        = ttraining, 
                                                              standardize = as.logical(params$scal_rlr), 
                                                              alpha       = params$alpha, 
                                                              family      = 'multinomial')}, 
                                       "lda"   = {
                                                 train.lda(var_, 
                                                           data = ttraining)}, 
                                       "qda"   = {
                                                 train.qda(var_, 
                                                           data = ttraining)}
                )
                if(length(category) == 2){
                  
                  # Obtiene la probabilidad de corte para el modelo
                  Corte     <- switch(models[j],
                                      "knnl"  = cortes$cvknnl_step, 
                                      "svml"  = cortes$cvsvml_step, 
                                      "dtl"   = cortes$cvdtl_step, 
                                      "xgb"   = cortes$cvxgb_step, 
                                      "rfl"   = cortes$cvrfl_step,
                                      "bl"    = cortes$cvbl_step, 
                                      "Bayes" = cortes$cvBayes_step, 
                                      "rl"    = cortes$cvrl_step, 
                                      "rlr"   = cortes$cvrlr_step, 
                                      "lda"   = cortes$cvlda_step, 
                                      "qda"   = cortes$cvqda_step)
                  # Obtiene la categoría de la variable a predecir seleccionada para aplicar probabilidad de corte
                  cat_sel   <- switch(models[j],
                                      "knnl"  = cortes$cvknnl_cat, 
                                      "svml"  = cortes$cvsvml_cat, 
                                      "dtl"   = cortes$cvdtl_cat, 
                                      "xgb"   = cortes$cvxgb_cat, 
                                      "rfl"   = cortes$cvrfl_cat,
                                      "bl"    = cortes$cvbl_cat, 
                                      "Bayes" = cortes$cvBayes_cat, 
                                      "rl"    = cortes$cvrl_cat, 
                                      "rlr"   = cortes$cvrlr_cat, 
                                      "lda"   = cortes$cvlda_cat, 
                                      "qda"   = cortes$cvqda_cat)
                  
                  # Se define la categoría positiva y negativa
                  # Categoría positiva se asume es la seleccionada 
                  positive    <- category[which(category == cat_sel)]
                  negative    <- category[which(category != cat_sel)]
                  
                  # Genera las probabilidades de predicción
                  prediccion  <- predict(modelo, ttesting, type = "prob")
                  # Guarda la clase verdadera
                  Clase       <- ttesting[,variable]
                  
                  # Obtiene las probabilidades para la categoría seleccionada
                  if(models[j] == "rlr")
                    Score       <- prediccion$prediction[,positive,]
                  else
                    Score       <- prediccion$prediction[,positive]
                  
                  # Genera la predicción con el corte y categoría seleccionada
                  Prediccion  <- ifelse(Score  > Corte, positive, negative)
                  # Crea la MC
                  MC          <- table(Clase , Pred = factor(Prediccion, levels = category))
                  # Suma la MC
                  MC.cv[[j]]  <- MC.cv[[j]] + MC
                }else{
                  # Para el caso de 3 o más categorías
                  # Predicción, MC 
                  prediccion  <- predict(modelo, ttesting)
                  MC          <- confusion.matrix(ttesting, prediccion)
                  MC.cv[[j]]  <- MC.cv[[j]] + MC
                }
                
              }
            } 
            
            # Guarda las matrices en la lista de matrices
            for (l in 1:length(MCs.cv)){
              MCs.cv[[l]][[i]] <- MC.cv[[l]]
            }
          }
          
          # Asigna los valores a las variables reactivas
          M$MCs.cv   <- MCs.cv
          # Se calculan los indices para realizar los gráficos
          resultados <- indices.cv(category, cant.vc, models, MCs.cv)
          M$grafico  <- resultados$grafico
          M$global   <- resultados$global
          M$categories <- resultados$categories
          M$times    <- 1
          isolate(codedioma$code <- append(codedioma$code, cv_cv_code(variable, dim_v, cant.vc, numGrupos)))
          
          print(MCs.cv)
          
        },error = function(e){
          return(e)
          #return(invisible(''))
        })
        
      })
    })
    
    
    # Gráfico de la precisión Global
    output$e_cv_glob  <-  renderEcharts4r({
      type    <- input$plot_type_p # Tipo de gráfico seleccionado
      grafico <- M$grafico # Datos del gráfico
      if(!is.null(grafico)){
        idioma    <- codedioma$idioma
        grafico$name <-  tr(grafico$name,idioma)
        
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(tr("precG",idioma), "" ), rotacion = TRUE)), 
                "error"  = return( resumen.error(grafico,  labels = c(tr("precG",idioma), tr("modelo", idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(grafico, labels = c(tr("precG",idioma),tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })    
    
    # Gráfico del error Global
    output$e_cv_error  <-  renderEcharts4r({
      idioma    <- codedioma$idioma
      type      <- input$plot_type_p # Tipo de gráfico seleccionado
      
      if(!is.null(M$grafico)){
        err  <- M$grafico # Datos del gráfico
        err$value <- 1 - M$global
        err$name <-  tr(err$name,idioma)
        switch (type,
                "barras" = return( resumen.barras(err, labels = c(tr("errG",idioma), "" ), rotacion = TRUE)), 
                "error"  = return( resumen.error(err,  labels = c(tr("errG",idioma), tr("modelo", idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                "lineas" = return( resumen.lineas(err, labels = c(tr("errG",idioma), tr("crossval",idioma) )))
        )
      }
      else
        return(NULL)
    })
    
    
    # Gráfico de precisión por categoría
    output$e_cv_category  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      tryCatch({
        cat    <- input$cv.cat.sel # Categoría seleccionada
        type   <- input$plot_type # Tipo de gráfico seleccionado
        if(!is.null(M$grafico)){
          graf  <- M$grafico
          graf$name <- tr(names(M$categories[[cat]]), codedioma$idioma)
          graf$value <- M$categories[[cat]]
          switch (type,
                  "barras" = return( resumen.barras(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), ""), rotacion = TRUE)), 
                  "error" = return( resumen.error(graf,   labels = c(paste0(tr("prec",idioma), " ",cat ), tr("modelo", idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                  "lineas" = return( resumen.lineas(graf, labels = c(paste0(tr("prec",idioma), " ",cat ), tr("crossval",idioma) )))
          )
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })
    })
    
    # Gráfico de error por categoría
    output$e_cv_category_err  <-  renderEcharts4r({
      idioma <- codedioma$idioma
      tryCatch({
        cat    <- input$cv.cat.sel # Categoría seleccionada
        type   <- input$plot_type # Tipo de gráfico seleccionado
        if(!is.null(M$grafico)){
          graf  <- M$grafico
          graf$name <-  tr(names(M$categories[[cat]]), codedioma$idioma)
          graf$value <- 1 - M$categories[[cat]]
          switch (type,
                  "barras" = return( resumen.barras(graf, labels = c(paste0("Error ",cat )), rotacion = TRUE)), 
                  "error" = return( resumen.error(graf,   labels = c(paste0("Error ",cat ), tr("modelo", idioma), tr("maximo", idioma),tr("minimo", idioma)))), 
                  "lineas" = return( resumen.lineas(graf, labels = c(paste0("Error ",cat ), tr("crossval",idioma) )))
          )
        }
        else
          return(NULL)
      },error = function(e){
        return(NULL)
      })
    })
    
    # Update Comparison Table
    output$TablaComp <- DT::renderDataTable({
      res        <- data.frame()
      idioma     <- codedioma$idioma
      global     <- M$grafico
      categorias <- M$categories
      tryCatch({
        global <- global |> 
          dplyr::group_by(name) |> 
          dplyr::summarise(value = mean(value))
        
        for (x in global$name) {
          new <- data.frame(
            OAccuracy = global[global$name == x, "value"],
            EAccuracy = 1- global[global$name == x, "value"]
          )
          for (cat in names(categorias)) {
            i <- which(names(categorias[[cat]]) == x)
            new[[paste0(tr("prec",idioma), " ",cat)]] <- mean(categorias[[cat]][i])
          }
          
          row.names(new) <- x
          res            <- rbind(res, new)
        }
        
        colnames(res)[1]           <- tr('precG', idioma)
        colnames(res)[2]           <- tr('errG', idioma)
        
        
        res[]                      <- lapply(res, as.numeric)
        res                        <- round(res * 100, 5)
        row.names(res) <- tr(row.names(res), codedioma$idioma)
        DT::datatable(res, selection = "none", editable = FALSE,
                      options = list(dom = "frtip", pageLength = 10, buttons = NULL))
      }, error = function(e) {
        showNotification(e, duration = 10)
        DT::datatable(data.frame(), selection = "none", editable = FALSE,
                      options = list(dom = "frtip", pageLength = 10, buttons = NULL))
      })
    },server = FALSE)
    
    
    #Actualiza la cantidad de capas ocultas (neuralnet)
    observeEvent(input$cant.capas.nn.pred, {
      if(!is.null(input$cant.capas.nn.pred)){
        for (i in 1:10) {
          if(i <= input$cant.capas.nn.pred) {
            shinyjs::show(paste0("nn.cap.pred.", i))
          } else {
            shinyjs::hide(paste0("nn.cap.pred.", i))
          }
        }
      }
    })
    
    # Obtiene los parámetros seleccionados para cada modelo
    listar_parametros <- function(){
      isolate({
        k_kn         <-  input$kmax.knn 
        scal_kn      <-  input$switch.scale.knn.pred 
        kernel_kn    <-  input$kernel.knn.pred 
        tipo_dt      <-  input$split.dt.pred 
        minsplit_dt  <-  input$minsplit.dt.pred 
        maxdepth_dt  <-  input$maxdepth.dt.pred 
        mtry         <-  input$mtry.rf.pred 
        tipo_rf      <-  input$split.rf.pred 
        ntree        <-  input$ntree.rf.pred 
        scal_svm     <-  input$switch.scale.svm.pred 
        kernel_svm   <-  input$kernel.svm.pred 
        tipo_xgb     <-  input$boosterXgb.pred 
        maxdepth_xgb <-  input$maxdepthXgb 
        n.rounds     <-  input$nroundsXgb 
        threshold    <-  input$threshold.nn 
        stepmax      <-  input$stepmax_nn 
        capas.np     <- c(input$nn.cap.pred.1 , input$nn.cap.pred.2 ,
                          input$nn.cap.pred.3 , input$nn.cap.pred.4 ,
                          input$nn.cap.pred.5 , input$nn.cap.pred.6 ,
                          input$nn.cap.pred.7 , input$nn.cap.pred.8 ,
                          input$nn.cap.pred.9 , input$nn.cap.pred.10 )
        cant.capas   <-  input$cant.capas.nn.pred 
        capas.np     <-  as.vector(as.numeric(capas.np[1:cant.capas] ))
        scal_rlr     <-  input$switch.scale.rlr.pred 
        alpha        <-  input$alpha.rlr.pred 
        iter         <-  input$iter.boosting.pred 
        maxdepth_b   <-  input$maxdepth.boosting.pred 
        minsplit_b   <-  input$minsplit.boosting.pred
        coeflearn_b  <- input$coeflearn
      })
      return(list(k_kn        = k_kn,        scal_kn     = scal_kn,     kernel_kn    = kernel_kn, 
                  tipo_dt     = tipo_dt,     minsplit_dt = minsplit_dt, maxdepth_dt  = maxdepth_dt, 
                  mtry        = mtry,        ntree       = ntree,       scal_svm     = scal_svm, 
                  kernel_svm  = kernel_svm,  tipo_xgb    = tipo_xgb,    maxdepth_xgb = maxdepth_xgb,  
                  n.rounds    = n.rounds,    threshold   = threshold,   stepmax      = stepmax, 
                  capas.np    = capas.np,    scal_rlr    = scal_rlr,    alpha        = alpha, 
                  iter        = iter,        maxdepth_b  = maxdepth_b,  minsplit_b   = minsplit_b, 
                  coeflearn_b = coeflearn_b, tipo_rf = tipo_rf))
    }
    
    # Obtiene las probabilidades de corte y categoría seleccionada para cada modelo
    parametros.prob.c <- function(){
      isolate({
        cvknnl_cat    <-  input$cvknnl_cat 
        cvknnl_step   <-  input$cvknnl_step 
        cvsvml_cat    <-  input$cvsvml_cat 
        cvsvml_step   <-  input$cvsvml_step
        cvdtl_cat     <-  input$cvdtl_cat 
        cvdtl_step    <-  input$cvdtl_step
        cvrfl_cat     <-  input$cvrfl_cat 
        cvrfl_step    <-  input$cvrfl_step
        cvxgb_cat     <-  input$cvxgb_cat 
        cvxgb_step    <-  input$cvxgb_step
        cvBayes_cat   <-  input$cvBayes_cat 
        cvBayes_step  <-  input$cvBayes_step
        cvbl_cat      <-  input$cvbl_cat 
        cvbl_step     <-  input$cvbl_step
        cvrl_cat      <-  input$cvrl_cat 
        cvrl_step     <-  input$cvrl_step 
        cvrlr_cat     <-  input$cvrlr_cat 
        cvrlr_step    <-  input$cvrlr_step 
        cvlda_cat     <-  input$cvlda_cat 
        cvlda_step    <-  input$cvlda_step 
        cvqda_cat     <-  input$cvqda_cat 
        cvqda_step    <-  input$cvqda_step 
      })
      return(list(cvknnl_cat = cvknnl_cat, cvknnl_step  = cvknnl_step, cvsvml_cat  = cvsvml_cat,  cvsvml_step   = cvsvml_step,
                  cvdtl_cat  = cvdtl_cat,  cvdtl_step   = cvdtl_step,  cvrfl_cat   = cvrfl_cat,   cvrfl_step    = cvrfl_step,
                  cvxgb_cat  = cvxgb_cat,  cvxgb_step   = cvxgb_step,  cvBayes_cat = cvBayes_cat, cvBayes_step  = cvBayes_step,
                  cvbl_cat   = cvbl_cat,   cvbl_step    = cvbl_step,   cvrl_cat    = cvrl_cat,    cvrl_step     = cvrl_step,
                  cvrlr_cat  = cvrlr_cat,  cvrlr_step   = cvrlr_step,  cvlda_cat   = cvlda_cat,   cvlda_step    = cvlda_step,
                  cvqda_cat  = cvqda_cat,  cvqda_step   = cvqda_step))
    }
    
    # Valores por defecto
    defaul_param_values <- function(){
      updateSliderInput(session, "cant.capas.nn.pred", value = 3)
    }
    
    #Actualiza y muestra las Categorías para Probabilidad de Corte
    actualizar.prob.corte <- function(choices){
      modelos <- c("knnl", "svml", "dtl", "rfl", "xgb", "Bayes", "bl", "rl", "rlr", "lda", "qda")

      if(length(choices) == 2){
        
        for (model in modelos) {
          #Actualiza categoría de ProbC
          updateSelectInput(session, paste0("cv", model, "_cat"), choices = choices, selected = choices[1]) 
          shinyjs::show(paste0("cv", model, "_cat"), anim = TRUE, animType = "fade")
          shinyjs::show(paste0("cv", model, "_step"), anim = TRUE, animType = "fade")
        }
      }else{
        for (model in modelos) {
          shinyjs::hide(paste0("cv", model, "_cat"), anim = TRUE, animType = "fade")
          shinyjs::hide(paste0("cv", model, "_step"), anim = TRUE, animType = "fade")
        }
      }
    }
}
    
## To be copied in the UI
# mod_cross_validation_ui("cross_validation_1")
    
## To be copied in the server
# mod_cross_validation_server("cross_validation_1")
