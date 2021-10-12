#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' dropNulls(list(1, NULL, 2))
dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv   <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

########################################################

load("inst/app/lang/translation.bin")

tr <- function(text, idioma = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
}

cambiar.labels <- function() {
  x <- c('acercade', 'activa', 'anterior', 'aplicar', 
      'buscar', 'cargar', 'cargarchivo', 'categorico', 'categoricas', 'coma',  
      'configuraciones', 'copyright', 'data', 'descarga', 'descargar', 'disyuntivo', 'espanol', 'eliminar',
      'header','idioma','imputar','info','key','no','nodata','numerico','numericas','ori','primero','punto',
      'puntocoma','salida','selidioma','selvar','selvars','semilla','separador','separadordec','si',
      'siguiente','subir','Rownames','res','tab','tipo','trans','ultimo','variables','version', 'deshabilitada', 
      'habilitada', 'seleccionarPredecir', 'propA', 'propP', 'VarsPred', 'generar', 'dataA', 'dataP','basico','resumen',
      'normalidad','dispersion','distribucion','correlacion','distribuciones','resumenvar','q1','q3','mediana','minimo'
      ,'maximo','promedio','ds','ejecutar','histograma','selcolbar','selcolline','selcolpoint','alfa',
      'plotnormal', 'selcolores','selcolor','curvanormal','tasim','pvalue','asimetria','sigue','copyright','poderpred',
      'distpred','distpredcat','pares','denspred','distrelvar','opciones','distpodcat','denspodlab','errornum',
      'errorcat', 'aprendizaje','knnl','generatem', 'predm','mc','indices','kmax','selkernel', 'escal','pred','reald','acerto','fallo',
      'prec','errG','precG', 'comparacion', 'tablaComp', 'selectMod', 'selectCat', 'rocCurva','svml', 'gclasificacion',
      'dtl', 'garbol', 'reglas', 'minsplit', 'maxdepth', 'splitIndex', 'numTree', 'numVars', 'ruleNumTree', 'evolerror',
      'varImp', 'rfl', 'NoDRule', 'xgb', 'selbooster', 'selnrounds', 'bl', 'selectAlg', 'nn', 'redPlot', 'threshold',
      'selectCapas', 'stepmax', 'nnWar', 'bigPlot', 'rl', 'rlr', 'landa', 'automatico', 'posibLanda', 'gcoeff', 'limitModel', 'aROC', 'RocNo',
      'predicnuevos', 'seleParModel', 'generarM', 'cargarNuev', 'NoTamColum', "eliminana", 'ErrorModelo', 'ErrorDatosPN', 'manual', 'home', 'atras',
      'resultados', 'lambda', 'limitLambda', 'superior', 'inferior', 'cargarComp', 'betas', 'cant', 'porcentaje')
  return(x)
}

# Función para generar diccionario.
# crear.traslation <- function() {
#    library(plyr)
#    archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#    translation <- dlply(archivo , .(key), function(s) key = as.list(s))
# 
#    save(translation, file = "translation.bin")
#  }
