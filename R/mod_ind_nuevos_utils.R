# -------------------  Prediccion Nuevos

#Elimina la información de newCases
borrar.datos <- function (newCases, prueba = FALSE){
  if(!prueba){
  newCases$originales        <- NULL
  newCases$datos.aprendizaje <- NULL
  newCases$variable.predecir <- NULL
  newCases$modelo            <- NULL
  newCases$m.seleccionado    <- NULL
  }
  newCases$prediccion        <- NULL
  newCases$datos.prueba      <- NULL
}

# Códigos de Modelos Ind.Nuevos--------------------------------------------------------------------------------------------------

# BAYES  
bayes.modelo.np <- function(variable.pr = ""){
  return(paste0("modelo.nuevos <<- train.bayes(",variable.pr,"~., data = datos.aprendizaje.completos)"))
}

# BOOSTING  
boosting.modelo.np <- function(variable.pr = NULL, iter = 50, maxdepth = 1,  minsplit = 1){
  iter     <- ifelse(!is.numeric(iter), 50, iter)
  maxdepth <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo   <- paste0("modelo.nuevos <<- train.adabag(",variable.pr,"~., data = datos.aprendizaje.completos, mfinal = ",iter,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

# DT
dt.modelo.np <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo   <- paste0("modelo.nuevos <<- train.rpart(",variable.pr,"~., data = datos.aprendizaje.completos,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))")
  return(codigo)
}

# KNN  
kkn.modelo.np <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  return(paste0("modelo.nuevos <<- traineR::train.knn(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

# RL  
rl.modelo.np <- function(variable.pr = ""){
  return(paste0("modelo.nuevos <<- traineR::train.glm(",variable.pr,"~., data = datos.aprendizaje.completos)"))
}

# NN
nn.modelo.np <- function(variable.pr = "",threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  capas     <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  stepmax   <- ifelse(1000>stepmax, 1000, stepmax)
  threshold <- ifelse(0.01>threshold, 0.01, threshold)
  
  return(paste0("modelo.nuevos <<- train.neuralnet(",variable.pr,"~., data = datos.aprendizaje.completos, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
                "threshold = ",threshold,", stepmax = ",stepmax,")\n"))
}

# RLR
rlr.modelo.np <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("modelo.nuevos <<- train.glmnet(",variable.pr,"~., data = datos.aprendizaje.completos, standardize = ",escalar,", alpha = ",alpha,", family = 'multinomial')"))
}

# RF
rf.modelo.np <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree  <- ifelse(!is.numeric(ntree), 500, ntree)
  Codigo <- paste0("modelo.nuevos <<- train.randomForest(",variable.pr,"~., data = datos.aprendizaje.completos,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(Codigo)
}

# SVM
svm.modelo.np <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.nuevos <<- traineR::train.svm(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kernel = '",kernel,"')"))
}


# XGBOOSTING
xgb.modelo.np <- function(variable.pr = "", booster = "gbtree", max.depth = 6, n.rounds = 60){
  return(paste0("modelo.nuevos <<- traineR::train.xgboost(",variable.pr,"~., data = datos.aprendizaje.completos, booster ='",booster,"', max_depth=",max.depth,", nrounds = ",n.rounds,")"))
}


