# Códigos de GX BOOSTING ---------------------------------------------------------------------------------------------------

#Crea el modelo
xgb.modelo <- function(variable.pr = NULL, booster = "gbtree",max.depth = 6, n.rounds = 60){
  return(paste0("modelo.xgb.",booster," <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}

#Código de la predicción de xgb
xgb.prediccion <- function(booster = "gbtree") {
  return(paste0("prediccion.xgb.",booster," <<- predict(modelo.xgb.",booster,", datos.prueba, type = 'class')"))
}


#Código de la matríz de confución de xgb
xgb.MC <- function(booster = "gbtree"){
  return(paste0("MC.xgb.",booster," <<- confusion.matrix(datos.prueba, prediccion.xgb.",booster,")","\n"))
}

#Código del grafico de importancia de variables
e_xgb_varImp <- function(booster = "gbtree"){
  paste0("nombres                   <- modelo.xgb.",booster,"$feature_names\n",   
         "variables.importantes     <- xgboost::xgb.importance(feature_names = nombres, model = modelo.xgb.",booster,")\n",
         "variables.importantes     <- variables.importantes[1:length(nombres),]","\n",
         "variables.importantes[,2] <- abs(variables.importantes[,2])","\n",
         "variables.importantes     <- na.omit(variables.importantes)\n",
         "datos.xgb <- data.frame(label = variables.importantes$Feature, values = variables.importantes[,2])\n",
         "datos.xgb |>  e_charts(label) |>  e_bar(values, name = var) |> \n",
         "e_tooltip() |>  e_datazoom(show = F) |>  e_show_loading()|> \n",
         "e_flip_coords()|> \n",
         "e_y_axis(inverse = TRUE)\n"
         
  )
}
