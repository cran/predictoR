
# FUNCIONES GLOBALES --------------------------------------------------------------------------------------------------------

#Crear la matriz de confucíon
crear.matriz.conf <- function(real, prediccion, num.clases = 2){
  MC <- table(real, prediccion)
  if (dim(MC)[2] < num.clases) {
    for (i in 1:(num.clases-dim(MC)[2])) {
      MC <- cbind(MC, 0)
    }
    colnames(MC) <- levels(real)
    row.names(MC) <- levels(real)
  }
  return(MC)
}

#Numero de categorias de la variable a predecir
num.categorias.pred <- function(){
  return(length(levels(datos.aprendizaje[,variable.predecir])))
}

num.categorias.pred.np <- function(){
  return(length(levels(datos.aprendizaje.completos[,variable.predecir.pn])))
}

#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

max.col <- function(m){
  base::max.col(apply(m, 1, function(x) max(x, na.rm = TRUE)) == m)
}

#Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

#Obtiene solo las variables numericas
var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Obtiene solo las variables categoricas
var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Genera un gauge
new.gauge <- function(id, val, lab){
  return(paste0("output$",id," <- renderGauge({
                gauge(round(",val,",2),
                min = 0, max = 100, symbol = '%',
                label = '",lab,"',
                gaugeSectors(success = c(0, 100)))})"))
}

# Genera los gauges
fill.gauges <- function(ids, indices) {
  titulos <- c(tr("precG"), tr("errG"))
  for (i in 1:length(ids)) {
    exe(new.gauge(ids[i], indices[[i]], titulos[i]))
  }
}

#Codigo del calculo de los indices
indices.generales <- function(MC) {
   if(1 == dim(MC)[2]) {
      MC <- cbind(MC, 0)
   }
   precision.global <- (sum(diag(MC)) / sum(MC)) * 100
   error.global <- (1 - (sum(diag(MC)) / sum(MC))) * 100
   precision.clase <- diag(MC)/rowSums(MC) * 100
   error.clase <- 100 - precision.clase
   return(list(precision.global = precision.global,
               error.global = error.global,
               precision.clase = precision.clase,
               error.clase = error.clase))
}

#Convierte una tabla de prediccion html a data.frame
dt.to.data.frame.predict <- function(datos){
  datos <- datos$x$data
  datos[,3] <- ifelse(datos[,1] == datos[,2], rep("Acertó",nrow(datos)), rep("Falló",nrow(datos)))
  return(datos)
}

#Crea la tabla de errores que se grafica en los indices de todos los modelos
indices.error.table <- function(indices, nombre = ""){
  err <- rbind(indices[[4]])
  colnames(err) <- paste0(c("Error."), colnames(err))
  row.names(err) <- nombre
  return(err)
}

#Crea la tabla de precisiones que se grafica en los indices de todos los modelos
indices.prec.table <- function(indices, nombre = ""){
  prec <- rbind(indices[[3]])
  colnames(prec) <- paste0(tr("prec"),".",colnames(prec))
  row.names(prec) <- nombre
  return(prec)
}

# Hace el grafico de la matriz de confusion
plot.MC.code <- function(cm) {
  return(paste0("
plot.MC <<- function(cm) {
  par(mar = c(2, 2, 2, 2))
  plot(c(1, 600), c(-100, 500), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  title('",tr("mc"),"', cex.main = 2)

  start <- 80
  len <- 500 - start

  n.class <- ncol(cm)
  names.class <- colnames(cm)
  prec.cat <- diag(cm) / rowSums(cm)
  error.cat <- 1 - prec.cat

  ancho <- len / n.class
  alto <- len / (n.class)
  x2 <- (x1 <- start) + ancho
  y2 <- (y1 <- len) - alto

  text(310, 485, '",tr("pred"),"', cex = 1.3, font = 2)
  text(start - 55, 250, 'Real', cex = 1.3, srt = 90, font = 2)

  for (i in 0:(n.class - 1)) {
    for (j in 0:(n.class - 1)) {
      x1.aux <- x1 + j * (ancho + 3)
      y1.aux <- y1 - i * (alto + 5)
      x2.aux <- x2 + j * (ancho + 3)
      y2.aux <- y2 - i * (alto + 5)
      if (j < (n.class)) {
        rect(x1.aux, y1.aux, x2.aux, y2.aux, col = ifelse(i == j, '#3f72af', '#11999e'))
        text(mean(c(x1.aux, x2.aux)),
          mean(c(y1.aux, y2.aux)),
          paste0(cm[(i + 1), (j + 1)], ' (', round(cm[(i + 1), (j + 1)] / sum(cm[(i + 1), ]), 2) * 100, '%)'),
          cex = 1.1, font = 2, col = 'white')
      }
    }
    text(mean(c((x2 + i * (ancho + 3)), (x1 + i * (ancho + 3)))), y1 + 20, names.class[i + 1], cex = 1)
    text(x1 - 20, mean(c((y1 - i * (alto + 5)), (y2 - i * (alto + 5)))), names.class[i + 1], cex = 1)
  }
  text(mean(c((x2 + (i + 1) * (ancho + 3)), (x1 + (i + 1) * (ancho + 3)))), y1 + 20, names.class[i + 2], cex = 1.2)
  text(mean(c((x2 + (i + 2) * (ancho + 3)), (x1 + (i + 2) * (ancho + 3)))), y1 + 20, names.class[i + 3], cex = 1.2)
  text(mean(c((x2 + (i + 3) * (ancho + 3)), (x1 + (i + 3) * (ancho + 3)))), y1 + 20, names.class[i + 4], cex = 1.2)
}"))
}

# Concatena y ejecuta un string como codigo
exe <- function(...){
  eval(parse(text = paste0(...)))
}

as.string.c <- function(vect, .numeric = FALSE){
  if(.numeric){
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
  else{
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
}

# createLog <- function(titulo = "", code = ""){
#   paste0("\n\n## ", titulo, "\n\n#### Interpretación\n```{r}\n", code, "\n```")
# }

extract.code <- function(funcion) {
  code <- paste(head(exe(funcion), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

# Pagina de Cargar y Transformar Datos --------------------------------------------------------------------------------------

#Transforma las variables a disyuntivas
datos.disyuntivos <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}

#Genera el codigo para cargar datos
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = T, d.o = "datos.originales", d = "datos" ){
  if(!is.null(ruta)){
    ruta <-  gsub("\\", "/", ruta, fixed=TRUE)
  }
  if(nombre.filas){
    return(paste0( d.o ," <<- read.table('", ruta, "', stringsAsFactors = T, header=",
                  encabezado, ", sep='", separador, "', dec = '", sep.decimal, "', row.names = 1) \n",d," <<- ",d.o))
  } else {
    return(paste0(d.o, "<<- read.table('", ruta, "', stringsAsFactors = T, header=", encabezado,
                  ", sep='", separador, "', dec = '", sep.decimal, "') \n",d," <<- ",d.o))
  }
}

#Eliminar NAs
code.NA <- function(deleteNA = T, d.o = "datos.originales") {
  res <- ifelse(deleteNA, paste0(d.o, "<<- na.omit(",d.o,")\n"),
                paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                       "for (variable in colnames(",d.o,")) {\n",
                       "  if(any(is.na(",d.o,"[, variable]))){\n",
                       "   ifelse(class(",d.o,"[, variable]) %in% c('numeric', 'integer'),\n",
                       "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                       "                                              mean(",d.o,"[, variable], na.rm = T),\n",
                       "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                       "                                     Mode(",d.o,"[, variable]))",
                       "\n   }\n}"))
  return(res)
}

#Genera el codigo para transformar datos
code.trans <- function(variable, nuevo.tipo, d.o = "datos.originales",d="datos"){
  if(nuevo.tipo == "categorico"){
    return(paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(d,"[, '", variable, "'] <<- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
  } else {
    es.factor <- ifelse( eval(parse(text = paste0("class(",d.o,"[, variable]) %in% c('numeric', 'integer')"))),
                        paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "']) \n"), "")
    return(paste0(es.factor, d, " <<- datos.disyuntivos(",d,", '", variable,"')"))
  }
}

#Desactiva las variables seleccionadas de los datos
code.desactivar <- function(variables, d = "datos"){
  return(paste0(d, " <<- subset(",d,", select = -c(", paste(variables, collapse = ","), "))"))
}

# Pagina de Segmentar Datos -------------------------------------------------------------------------------------------------

#Crea el código de la particion en testing y learning data
particion.code <- function(data = "datos", p = "0.5", variable = NULL, semilla = 5, perm.semilla = FALSE){
  variable.predecir <<- variable
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  codigo <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), "rm(.Random.seed, envir = globalenv())")
  codigo <- paste0(codigo,"\nvariable.predecir <<- '",variable,
  "'\nparticion <- sample(1:nrow(datos),size = nrow(datos)*",p/100,", replace = FALSE)\n
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]")
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

# Pagina de Resumen ---------------------------------------------------------------------------------------------------------

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

#Genera el resumen numerico de una variable
resumen.numerico <- function(data, variable) {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", tr("q1")), color = "green",
      Value = format(round(quantile(data[, variable], .25), 3), scientific = F)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", tr("mediana")),
      Value = format(round(median(data[, variable]), 3), scientific = F),
      color = "orange"),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", tr("q3")), color = "maroon",
      Value = format(round(quantile(data[, variable], .75), 3), scientific = F)
    ),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", tr("minimo")),
      Value = format(round(min(data[, variable]), 3), scientific = F),
      color = "red"),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", tr("promedio")),
      Value = format(round(mean(data[, variable]), 3), scientific = F),
      color = "blue"),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", tr("maximo")),
      Value = format(round(max(data[, variable]), 3), scientific = F),
      color = "purple"),
    DS <- list(
      id = "ds", Label = tags$span(`data-id`="ds", tr("ds")), color = "yellow",
      Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3)
    )
  )

  res <- lapply(datos.numericos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=i$id,
      tags$div(
        class=paste0('small-box bg-', i$color),
        tags$div(class='inner', tags$h3(i$Value), tags$p(i$Label)),
        tags$div(class='icon-large', tags$i(class=i$icon))
      )
    )
  })
  return(res)
}

#Genera el resumen categorico de una variable
resumen.categorico <- function(data, variable){
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon",
             "black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- levels(data[, variable])
  res <- lapply(datos.categoricos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=paste0(variable, i),
      tags$div(
        class=paste0('small-box bg-', sample(color, 1)),
        tags$div(class='inner', tags$h3(summary(data[, variable])[i]), tags$p(i))
      )
    )
  })
  return(res)
}

# Pagina del Test de Normalidad ---------------------------------------------------------------------------------------------

#Codigo de la genracion de la curva normal (test de normalidad)
default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA", labelcurva = "Curva Normal"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = F)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
      "  freq = F, ylim = range(0, max(values)), ylab = '',xlab = '",vars,"', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("argument 'x' is must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)

  sk <- sum((x - mean(x))^3/stats::sd(x)^3)/nx

  return(sk)
}

#Genera  la tabla de normalidad
default.calc.normal <- function(data = "datos", labelsi = "Positiva", labelno = "Negativa",labelsin = "Sin Asimetría") {
  return(paste0(
    "calc <- lapply(var.numericas(", data,"), function(i) fisher.calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, '", labelsi,
    "',\n  ifelse(i < 0, '", labelno, "', '", labelsin, "')))) \n",
    "calc <- t(calc)\ncalc"))
}

# Pagina de Dispersion ------------------------------------------------------------------------------------------------------

#Codigo del grafico de dispersion
default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return(NULL)
  } else if(length(vars) == 2) {
    return(paste0("ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(", data, "))) +
geom_point(color = '", color, "', size = 3) + geom_text(vjust = -0.7) + theme_minimal()"))
  } else{
    return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

# Pagina de Distribucion ----------------------------------------------------------------------------------------------------

#Llama a la funcion que crea la distribuccion numerica
def.code.num <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

#Llama a la funcion que crea la distribuccion categorica
def.code.cat <- function(data = "datos", variable, titulox = tr("cantidadcasos"), tituloy = tr("categorias")) {
  paste0("distribucion.categorico(", data, "[, '", variable,"']) + ",
         "labs(title = '", variable, "', x = '",titulox, "', y = '", tituloy, "')")
}

#Hace el grafico de la distribucion numerica
distribucion.numerico <- function(var, nombre.var, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(var, col = color, border=F, axes=F, main = nombre.var)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3,
          boxwex = 1.5, edcol = color, medlty = 1, medlwd = 8, axes=F,
          medcol = color, whiskcol = color, whisklty = 3, staplecol = color,
          staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red')
}

#Hace el grafico de la distribucion categorica
distribucion.categorico <- function(var) {
  colores <- sapply(levels(var),
                    function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
    theme_minimal()
}

# Pagina de Correlacion -----------------------------------------------------------------------------------------------------

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

#Codigo de la generacion de correlaciones
correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot::corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

# Pagina de Poder Predictivo ------------------------------------------------------------------------------------------------

#Calcula proporciones
dist.x.predecir <- function(data, variable, variable.predecir) {
  if(variable != variable.predecir){
    data <- data %>%
      dplyr::group_by_at(c(variable, variable.predecir)) %>%
      dplyr::summarise(count = n())
  } else {
    data <- data %>% dplyr::group_by_at(variable) %>%
      dplyr::summarise(count = n())
  }
  data <- data %>% dplyr::mutate(prop = round(count/sum(count),4))
  return(data)
}

#Hace la grafica de proporciones segun la variable predictiva
plot.code.dist.porc <- function(variable, var.predecir, colores = NA, label.size = 9.5, label = "${X} ${Y}"){
  label = str_interp(label,list(X=variable,Y=var.predecir))
  return(paste0("colores <- gg_color_hue(length(unique(datos[,'",var.predecir,"'])))
label.size <- ",label.size," - length(unique(datos[,'",variable,"']))
label.size <- ifelse(label.size < 3, 3, label.size)
data <- dist.x.predecir(datos, '",variable,"', '",var.predecir,"')
ggplot(data, aes(fct_reorder(data[['",variable,"']], count, .desc = T), prop, fill = data[['",var.predecir,"']])) +
geom_bar(stat = 'identity') +
geom_text(aes(label = paste0(count, ' (', scales::percent(prop), ')'), y = prop), color = 'black',
position = position_stack(vjust = .5), size = label.size) +
theme_minimal() +
theme(text = element_text(size=15)) +
scale_fill_manual(values = colores) +
scale_y_continuous(labels = scales::percent)+
coord_flip() +
labs(title = '",label,"', x = '', y = '') +
guides(fill = guide_legend(reverse=T)) +
theme(legend.position = 'top', legend.title = element_blank())
"))
}

plot.code.poder.pred <- function(var.predecir, label.size = 9.5, label=""){
  return(paste0("colores <- gg_color_hue(length(unique(datos[,'",var.predecir,"'])))
label.size <- ",label.size," - length(unique(datos[,'",var.predecir,"']))
label.size <- ifelse(label.size < 3, 3, label.size)
data. <- dist.x.predecir(datos, '",var.predecir,"','",var.predecir,"')
ggplot(data., aes(x='', y=prop, fill= data.[['",var.predecir,"']]))+
geom_bar(width = 1, stat = 'identity')+
geom_text(aes(label = paste0(count, ' (', scales::percent(prop), ')'), y = prop ), color = 'black',
position = position_stack(vjust = .5), size = label.size)+
theme_minimal() +
theme(text = element_text(size=15)) +
scale_fill_manual(values = colores) +
scale_y_continuous(labels = scales::percent)+
coord_flip()+
labs(title = '",label," \"",var.predecir,"\"', x = '', y = '') +
guides(fill = guide_legend(reverse=T)) +
theme(legend.position = 'top', legend.title = element_blank())
"))
}

#Grafica el pairs
pairs.poder <- function(){
  return(paste0("vars.p <- datos[,'",variable.predecir,"']
col <- rainbow((length(unique(vars.p)) + 1)*2)[seq(2,(length(unique(vars.p)) + 1)*2,2)]
col <- col[2:length(col)]
pairs.panels(var.numericas(datos),bg = col[datos[,'",variable.predecir,"']],
pch= 22, main='', hist.col = gg_color_hue(1), ellipses = FALSE, oma=c(3,3,3,15))
legend('topright', fill = unique(col[datos[,'",variable.predecir,"']]), legend = c(levels(datos[,'",variable.predecir,"'])))"))
}

#Grafica la densidad de las variables numericas
plot.numerico.dens <- function(variable, label = "${X} ${Y}"){
  label = str_interp(label,list(X=variable,Y=variable.predecir))
  return(paste0("ggplot(datos, aes_string('",variable,"', fill = '",variable.predecir,"')) +
geom_density( alpha = .85) +
theme_minimal() +
theme(text = element_text(size=15)) +
scale_fill_manual(values = gg_color_hue(length(levels(datos[,'",variable.predecir,"'])))) +
labs(title = '",label,"', y = '', x = '') +
theme(legend.position = 'top', legend.title = element_blank(), text = element_text(size = 15))"))
}

# Pagina de KNN -------------------------------------------------------------------------------------------------------------

#Crea el modelo KNN
kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  return(paste0("modelo.knn.",kernel," <<- train.kknn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

kkn.modelo.np <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  return(paste0("modelo.nuevos <<- train.kknn(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de knn
kkn.prediccion <- function(kernel = "optimal") {
  return(paste0("prediccion.knn.",kernel," <<- predict(modelo.knn.",kernel,", datos.prueba[,-which(colnames(datos.prueba) == '",variable.predecir,"')])"))
}

kkn.prediccion.pn <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de knn
knn.MC <- function(variable.p, kernel = "optimal"){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.knn.",kernel,"\n",
                "MC.knn.",kernel," <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")"))
}

# Pagina de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm.",kernel," <<- svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')"))
}

svm.modelo.np <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.nuevos <<- svm(",variable.pr,"~., data = datos.aprendizaje.completos, scale =",scale,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de svm
svm.prediccion <- function(kernel = "linear") {
  return(paste0("prediccion.svm.",kernel," <<- predict(modelo.svm.",kernel," , datos.prueba[,-which(colnames(datos.prueba) == '",variable.predecir,"')])"))
}

svm.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos , datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de svm
svm.MC <- function(variable.p, kernel = "linear"){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.svm.",kernel,"\n",
                "MC.svm.",kernel," <<- crear.matriz.conf(real,prediccion,",num.categorias.pred(),")"))
}

#Codigo del grafico de svm
svm.plot <- function(variables, resto, kernel = "linear"){
  if(is.null(variables)){
    return("NULL")
  }
  l <- c()
  for(i in 1:length(resto)){
    l <- c(l , paste0(resto[i]," = ", i))
  }
  l <- paste0("list(",paste0(l,collapse = ","),")")
  s <- paste0("modelo.svm.temp <- svm(",variable.predecir,"~",variables[1],"+",variables[2],", data = datos.aprendizaje, kernel = '",kernel,"')")
  color <- length(unique(datos.aprendizaje[,variable.predecir]))
  color <- as.string.c(gg_color_hue(color))
  return(paste0(s,"\nplot(modelo.svm.temp, datos, ",variables[1],"~",variables[2],", slice = ",l,", col = ",color,")"))
}


# Pagina de DT --------------------------------------------------------------------------------------------------------------

#Crea el modelo DT
dt.modelo <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo <- paste0("modelo.dt.",split," <<- rpart(",variable.pr,"~., data = datos.aprendizaje,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))")
  return(codigo)
}

dt.modelo.np <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15, split = "gini"){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo <- paste0("modelo.nuevos <<- rpart(",variable.pr,"~., data = datos.aprendizaje.completos,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"),parms = list(split = '",split,"'))")
  return(codigo)
}

#Codigo de la prediccion de DT
dt.prediccion <- function() {
  return(paste0("prediccion.dt.",input$split.dt," <<- predict(modelo.dt.",input$split.dt,", datos.prueba, type='class')"))
}

dt.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type='class')"))
}

#Codigo de la matriz de confucion de dt
dt.MC <- function(variable.p){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.dt.",input$split.dt,"\n",
                "MC.dt.",input$split.dt," <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")"))
}

#Codigo del grafico de dt
dt.plot <- function(){
  num <- length(levels(datos[,variable.predecir]))
  return(paste0("prp(modelo.dt.",input$split.dt,", type = 2, extra = 104, nn = T, varlen = 0, faclen = 0,
fallen.leaves = TRUE, branch.lty = 6, shadow.col = 'gray82',
box.col = gg_color_hue(",num,")[modelo.dt.",input$split.dt,"$frame$yval])"))
}

# Pagina de RF --------------------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.rf <<- randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

rf.modelo.np <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.nuevos <<- randomForest(",variable.pr,"~., data = datos.aprendizaje.completos,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

#Codigo de la prediccion de rf
rf.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.rf <<- predict(modelo.rf,datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

rf.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de rf
rf.MC <- function(variable.p){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.rf\n",
                "MC.rf <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n"))
}

#Codigo del grafico de importancia de variables
rf.plot <- function() {
  return(paste0(
    "aux <- data.frame(modelo.rf$importance)\n",
    "aux <- aux[order(aux$MeanDecreaseAccuracy, decreasing = T), ]\n",
    "aux$nombre <- row.names(aux)\n\n",
    "ggplot(aux, aes(x = MeanDecreaseAccuracy, y = fct_reorder(nombre, MeanDecreaseAccuracy))) +\n",
    "  geom_bar(stat = 'identity', fill = 'steelblue') + labs(y = '') +\n",
    "  theme_minimal()\n"
  ))
}

#Codigo del grafico de error del modelo
plot.rf.error <- function(){
  return(paste0("plot(modelo.rf, main='')\n",
         "legend('topright', c('OOB','",
         paste0(unique(datos[,variable.predecir]), collapse = "','"), "'), text.col=1:6, lty=1:5, col=1:6)"))
}

# Pagina de BOOSTING --------------------------------------------------------------------------------------------------------

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, maxdepth = 1, type = "discrete", minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.boosting.",type," <<- ada(",variable.pr,"~., data = datos.aprendizaje, iter = ",iter,", type = '",type,"',
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

boosting.modelo.np <- function(variable.pr = NULL, iter = 50, maxdepth = 1, type = "discrete", minsplit = 1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  nu <- ifelse(!is.numeric(maxdepth) && maxdepth > 30, 15, maxdepth)
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  codigo <- paste0("modelo.nuevos <<- ada(",variable.pr,"~., data = datos.aprendizaje.completos, iter = ",iter,", type = '",type,"',
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ",maxdepth,"))")
  return(codigo)
}

#Codigo de la prediccion de boosting
boosting.prediccion <- function(variable.pr = NULL, type = "discrete") {
  return(paste0("prediccion.boosting.",type," <<- predict(modelo.boosting.",type,", datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

boosting.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de boosting
boosting.MC <- function(variable.p, type = "discrete"){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.boosting.",type,"\n",
                "MC.boosting.",type," <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n"))
}

#Codigo del grafico de boosting
boosting.plot <- function(type = "discrete"){
  return(paste0("plot(modelo.boosting.",type,")"))
}

#Codigo del grafico de boosting
boosting.plot.import <- function(type = "discrete"){
  return(paste0("varP(modelo.boosting.",type,")"))
}

rules.boosting <- function(type = "discrete", i){
  return(paste0("rules(modelo.boosting.",type,"$model$trees[[",i,"]])"))
}

varP <- function (x, plot.it = TRUE, type = c("none", "scores"), max.var.show = 30, ...){
  if (class(x) != "ada") {
    stop("Object must be of type 'ada'")
  }
  if (missing(type)) {
    type = "none"
  }
  iter <- x$iter
  nm <- x$names
  vec <- rep(0, length(nm))
  p = x$dim[2]
  g1 <- function(i, obj) {
    if (dim(obj[[i]]$frame)[1] < 2) {
      return(rep(0, p))
    }
    imp <- obj[[i]]$splits[, 3]^2
    vals <- match(row.names(obj[[i]]$splits), nm)
    vec = rep(0, p)
    vec[vals] <- imp
    vec
  }
  vec <- 1/iter * sqrt(apply(sapply(1:iter, function(i) g1(i,
                                                           x$model$trees)), 1, sum))
  vars <- order(vec, decreasing = TRUE)
  n <- length(vec)
  max.v = max.var.show
  if (p < max.v)
    max.v = p
  if (plot.it == TRUE) {
    df <- data.frame( val = vec[vars[max.v:1]], label = nm[vars[max.v:1]]) %>%
      dplyr::mutate(label = forcats::fct_reorder(label, val, .desc = FALSE))

    print(ggplot(df, ggplot2::aes(x = label, y = val, fill = label)) +
            geom_bar(stat = "identity", position = "identity", width = 0.1) +
            labs(title = tr("varp"),  y = "", x = "") +
            scale_y_continuous(labels = scales::comma) +
            coord_flip() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none"))
  }
  if (type == "scores") {
    vars = vars[1:max.v]
    t1 <- vec[vars]
    attr(t1, "names") <- nm[vars]
    return(t1)
  }
}

rules <- function (model, compact = FALSE, ...){
  if (!inherits(model, "rpart"))
    stop(rattle:::Rtxt("Not a legitimate rpart tree"))
  rtree <- length(attr(model, "ylevels")) == 0
  target <- as.character(attr(model$terms, "variables")[2])
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1, ]$n
  if (rtree)
    ordered <- rev(sort(frm$n, index = TRUE)$ix)
  else ordered <- rev(sort(frm$yval2[, 5], index = TRUE)$ix)
  for (i in ordered) {
    if (frm[i, 1] == "<leaf>") {
      if (rtree)
        yval <- frm[i, ]$yval
      else {
        yval <- as.numeric(ylevels[frm[i, ]$yval])
        yval <- ifelse(yval == -1, 1, 2)
        yval <- levels(datos.aprendizaje[,variable.predecir])[yval]
      }
      cover <- frm[i, ]$n
      pcover <- round(100 * cover/ds.size)
      if (!rtree)
        prob <- frm[i, ]$yval2[, 5]
      cat("\n")
      pth <- rpart::path.rpart(model, nodes = as.numeric(names[i]),
                               print.it = FALSE)
      pth <- unlist(pth)[-1]
      if (!length(pth))
        pth <- "True"
      if (compact) {
        cat(sprintf("R%03s ", names[i]))
        if (rtree)
          cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        else cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
        cat(sprintf(" %s", pth), sep = "")
      }
      else {
        cat(sprintf(rattle:::Rtxt("Rule number: %s "), names[i]))
        if (rtree){
          cat(sprintf("[%s=%s cover=%d (%.0f%%)]\n", target, yval, cover, pcover))
        }else{
          cat(sprintf("[%s=%s cover=%d (%.0f%%) prob=%0.2f]\n", target, yval, cover, pcover, prob))
        }
        cat(sprintf("  %s\n", pth), sep = "")
      }
    }
  }
  cat("\n")
  invisible(ordered)
}

# Pagina de BAYES ---------------------------------------------------------------------------------------------------------

#Crea el modelo Bayes
bayes.modelo <- function(){
  return(paste0("modelo.bayes <<- naiveBayes(",variable.predecir,"~., data = datos.aprendizaje)"))
}

bayes.modelo.np <- function(variable.pr = ""){
  return(paste0("modelo.nuevos <<- naiveBayes(",variable.pr,"~., data = datos.aprendizaje.completos)"))
}

#Codigo de la prediccion de Bayes
bayes.prediccion <- function() {
  return(paste0("prediccion.bayes <<- predict(modelo.bayes, datos.prueba[,-which(colnames(datos.prueba) == '",variable.predecir,"')])"))
}

bayes.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la matriz de confucion de Bayes
bayes.MC <- function(){
  return(paste0("real <- datos.prueba$",variable.predecir,"\n",
                "prediccion <- prediccion.bayes\n",
                "MC.bayes <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n"))
}

# Pagina de NN ------------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo <- function(threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  selector <- -which(colnames(datos.aprendizaje) == variable.predecir)
  foemula.1 <- paste0(paste0("`",levels(datos.aprendizaje[,variable.predecir]),"`"), collapse = "+")

  paste0("datos.dummies.apren <- as.data.frame(scale(dummy.data.frame(datos.aprendizaje[,",selector,"])))\n",
         "datos.dummies.apren['",variable.predecir,"'] <- datos.aprendizaje[,'",variable.predecir,"']\n",
         "datos.dummies.apren <- datos.dummies.apren %>% dplyr::mutate(.valor.nuevo = TRUE,i = row_number()) %>%\n",
         "\t\t\t\t\t\ttidyr::spread(key = ",variable.predecir,", value='.valor.nuevo', fill = FALSE) %>% dplyr::select(-i)\n",
         "nombres <- colnames(datos.dummies.apren)\n",
         "formula.nn <- as.formula(paste('",foemula.1,
         "~', paste0(nombres[!nombres %in% ",as.string.c(levels(datos.aprendizaje[,variable.predecir])),"], collapse = '+')))\n",
         "modelo.nn <<- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
         "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

nn.modelo.np <- function(variable.pr = "",threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  stepmax <- ifelse(1000>stepmax, 1000, stepmax)
  threshold <- ifelse(0.01>threshold, 0.01, threshold)
  selector <- -which(colnames(datos.aprendizaje.completos) == variable.pr)
  foemula.1 <- paste0(paste0("`",levels(datos.aprendizaje.completos[,variable.pr]),"`"), collapse = "+")

  paste0("datos.dummies.apren <- as.data.frame(scale(dummy.data.frame(datos.aprendizaje.completos[,",selector,"])))\n",
         "datos.dummies.apren['",variable.pr,"'] <- datos.aprendizaje.completos[,'",variable.pr,"']\n",
         "datos.dummies.apren <- datos.dummies.apren %>% dplyr::mutate(.valor.nuevo = TRUE,i = row_number()) %>%\n",
         "\t\t\t\ttidyr::spread(key = ",variable.pr,", value='.valor.nuevo', fill = FALSE) %>% dplyr::select(-i)\n",
         "nombres <- colnames(datos.dummies.apren)\n",
         "formula.nn <- as.formula(paste('",foemula.1,
         "~', paste0(nombres[!nombres %in% ",as.string.c(levels(datos.aprendizaje.completos[,variable.pr])),"], collapse = '+')))\n",
         "modelo.nuevos <<- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = FALSE,",
         "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

#Codigo de la prediccion de xgb
nn.prediccion <- function() {
  selector <- -which(colnames(datos.prueba) == variable.predecir)

  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(datos.prueba[,",selector,"])))\n",
         "datos.dummies.prueb['",variable.predecir,"'] <- NULL\n",
         "prediccion.nn <<- neuralnet::compute(modelo.nn, datos.dummies.prueb)$net.result\n",
         "max.col(prediccion.nn)")
}

nn.prediccion.np <- function(){
  selector <- -which(colnames(datos.prueba.completos) == variable.predecir.pn)

  clases <- levels(datos.aprendizaje.completos[,variable.predecir.pn])
  num.class <- length(clases)
  x <- paste0("'",1:num.class,"'='%s'",collapse = ",")
  recod <- do.call(sprintf, c(list(x), clases))

  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(datos.prueba.completos[,",selector,"])))\n",
         "datos.dummies.prueb['",variable.predecir.pn,"'] <- NULL\n",
         "predic.nuevos <<- neuralnet::compute(modelo.nuevos, datos.dummies.prueb)$net.result\n",
         "predic.nuevos <<- max.col(predic.nuevos)\n",
         "predic.nuevos <<- recode(predic.nuevos, ",recod,")")
}

#Codigo de la matriz de confucion de xgb
nn.MC <- function(){
  paste0("prediccion <- factor(max.col(prediccion.nn), levels = 1:length(levels(datos.prueba[,'",variable.predecir,"'])))\n",
         "real <- as.numeric(datos.prueba[,'",variable.predecir,"'])\n",
         "MC.nn <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n",
         "rownames(MC.nn) <<- ",as.string.c(levels(datos.prueba[, variable.predecir])),"\n",
         "colnames(MC.nn) <<- ",as.string.c(levels(datos.prueba[, variable.predecir])))
}

nn.plot <- function(){
  paste0("plot(modelo.nn,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# Pagina de GX BOOSTING ---------------------------------------------------------------------------------------------------

#Crea el modelo GX BOOSTING
xgb.modelo <- function(booster = "gbtree",max.depth = 6, n.rounds = 60){
  num.class <- length(levels(datos.aprendizaje[,variable.predecir]))
  if(num.class > 2){
    tipo <- "multi:softprob"
    str.num.class <- paste0(",num_class =",num.class)
    eval.xgb <- "mlogloss"
  }else{
    tipo <- "binary:logistic"
    str.num.class <- ""
    eval.xgb <- "error"
  }
  categoricas <- colnames(var.categoricas(datos.aprendizaje))
  categoricas <- paste0(categoricas,collapse = "','")
  matrices <- paste0("d.aprendizaje <- mutate_all(datos.aprendizaje, funs(as.numeric))\n",
                     "d.aprendizaje[,c('",categoricas,"')] <- d.aprendizaje[,c('",categoricas,"')] - 1\n",
                     "selector <- -which(colnames(d.aprendizaje) == '",variable.predecir,"')\n",
                     "mxgb.aprendizaje <- xgb.DMatrix(data = data.matrix(d.aprendizaje[,selector]),",
                     "label = data.matrix(d.aprendizaje$'",variable.predecir,"'))\n")
  parametros <- paste0("parametros <- list(booster = '",booster,"', objective = '",tipo,"', max_depth=",max.depth, str.num.class,")\n")
  modelo <- paste0("modelo.xgb.",booster," <<- xgb.train(params = parametros, data = mxgb.aprendizaje, nrounds =",n.rounds,",verbose = 0,
                   watchlist = list(train=mxgb.aprendizaje), early_stop_round = 10, maximize = F , eval_metric = '",eval.xgb,"')")
  return(paste0(matrices,parametros,modelo))
}

xgb.modelo.np <- function(variable.pr = "", booster = "gbtree", max.depth = 6, n.rounds = 60){
  num.class <- length(levels(datos.aprendizaje.completos[,variable.pr]))
  if(num.class > 2){
    tipo <- "multi:softprob"
    str.num.class <- paste0(",num_class =",num.class)
    eval.xgb <- "mlogloss"
  }else{
    tipo <- "binary:logistic"
    str.num.class <- ""
    eval.xgb <- "error"
  }
  categoricas <- colnames(var.categoricas(datos.aprendizaje.completos))
  categoricas <- paste0(categoricas,collapse = "','")
  matrices <- paste0("d.aprendizaje <- mutate_all(datos.aprendizaje.completos, funs(as.numeric))\n",
                     "d.aprendizaje[,c('",categoricas,"')] <- d.aprendizaje[,c('",categoricas,"')] - 1\n",
                     "selector <- -which(colnames(d.aprendizaje) == '",variable.pr,"')\n",
                     "mxgb.aprendizaje <- xgb.DMatrix(data = data.matrix(d.aprendizaje[,selector]),",
                     "label = data.matrix(d.aprendizaje$'",variable.pr,"'))\n")
  parametros <- paste0("parametros <- list(booster = '",booster,"', objective = '",tipo,"', max_depth=",max.depth, str.num.class,")\n")
  modelo <- paste0("modelo.nuevos <<- xgb.train(params = parametros, data = mxgb.aprendizaje, nrounds =",n.rounds,",verbose = 0,
                   watchlist = list(train=mxgb.aprendizaje), early_stop_round = 10, maximize = F , eval_metric = '",eval.xgb,"')")
  return(paste0(matrices,parametros,modelo))
}

#Codigo de la prediccion de xgb
xgb.prediccion <- function(booster = "gbtree") {
  categoricas <- colnames(var.categoricas(datos.aprendizaje))
  categoricas <- paste0(categoricas,collapse = "','")
  pre <- paste0("d.prueba <- mutate_all(datos.prueba, funs(as.numeric))\n",
                "valor.var.xgb.",booster," <<- d.prueba[,'",variable.predecir,"']\n",
                "d.prueba[,c('",categoricas,"')] <- d.prueba[,c('",categoricas,"')] - 1\n",
                "selector <- -which(colnames(d.prueba) == '",variable.predecir,"')\n",
                "mxgb.prueba <- xgb.DMatrix(data = data.matrix(d.prueba[,selector])\n,",
                "label = data.matrix(d.prueba$'",variable.predecir,"'))\n")
  return(paste0(pre, "prediccion.xgb.",booster," <<- predict(modelo.xgb.",booster,", mxgb.prueba)"))
}

xgb.prediccion.np <- function() {
  clases <- levels(datos.aprendizaje.completos[,variable.predecir.pn])
  num.class <- length(clases)
  if(num.class > 2){
    pred <- paste0("predic.nuevos <<- max.col(matrix(predic.nuevos, ncol=",num.class,", byrow=TRUE))\n")
  }else{
    pred <- paste0("predic.nuevos <<- ifelse(predic.nuevos > 0.5, 2, 1)\n")
  }
  x <- paste0("'",1:num.class,"'='%s'",collapse = ",")
  recod <- do.call(sprintf, c(list(x), clases))

  categoricas <- colnames(var.categoricas(datos.prueba.completos))
  categoricas <- categoricas[variable.predecir.pn != categoricas]
  if(length(categoricas) > 0){
    categoricas <- paste0(categoricas,collapse = "','")
    categoricas <- paste0("d.prueba[,c('",categoricas,"')] <- d.prueba[,c('",categoricas,"')] - 1\n")
  }else{
    ategoricas <- ""
  }
  pre <- paste0("d.prueba <- mutate_all(datos.prueba.completos, funs(as.numeric))\n",
                categoricas,
                "selector <- -which(colnames(d.prueba) == '",variable.predecir.pn,"')\n",
                "mxgb.prueba <- xgb.DMatrix(data = data.matrix(d.prueba[,selector]),",
                "label = data.matrix(d.prueba$'",variable.predecir.pn,"'))\n",
                "predic.nuevos <<- predict(modelo.nuevos, mxgb.prueba)\n",
                pred,
                "predic.nuevos <<- recode(predic.nuevos, ",recod,")")
  return(pre)
}

#Codigo de la matriz de confucion de xgb
xgb.MC <- function(booster = "gbtree"){
  num.class <- length(levels(datos.aprendizaje[,variable.predecir]))
  if(num.class > 2){
    pred <- paste0("prediccion.xgb.",booster," <- matrix(prediccion.xgb.",booster,", ncol=",num.class,", byrow=TRUE)\n",
                   "prediccion.xgb.",booster," <- max.col(prediccion.xgb.",booster,")\n")
  }else{
    pred <- paste0("prediccion.xgb.",booster," <- ifelse(prediccion.xgb.",booster," > 0.5, 2, 1)\n")
  }
  paste0(pred,
         "real <- valor.var.xgb.",booster,"\n",
         "prediccion <- prediccion.xgb.",booster,"\n",
         "MC.xgb.",booster," <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n",
         "rownames(MC.xgb.",booster,") <<- ",as.string.c(levels(datos.prueba[, variable.predecir])),"\n",
         "colnames(MC.xgb.",booster,") <<- ",as.string.c(levels(datos.prueba[, variable.predecir])))
}

#Codigo interno del grafico de importancia
xgb.plot.importance <- function (importance_matrix = NULL, top_n = NULL, measure = NULL, rel_to_first = FALSE,
                                 left_margin = 10, cex = NULL, plot = TRUE, ...){
  xgboost:::check.deprecation(...)
  if (!data.table::is.data.table(importance_matrix)) {
    stop("importance_matrix: must be a data.table")
  }
  imp_names <- colnames(importance_matrix)
  if (is.null(measure)) {
    if (all(c("Feature", "Gain") %in% imp_names)) {
      measure <- "Gain"
    }
    else if (all(c("Feature", "Weight") %in% imp_names)) {
      measure <- "Weight"
    }
    else {
      stop("Importance matrix column names are not as expected!")
    }
  }
  else {
    if (!measure %in% imp_names)
      stop("Invalid `measure`")
    if (!"Feature" %in% imp_names)
      stop("Importance matrix column names are not as expected!")
  }
  importance_matrix <- importance_matrix[, `:=`(Importance,
                                                sum(get(measure))), by = Feature]
  importance_matrix <- importance_matrix[, `:=`(Importance, mean(get("Importance"))), by = Feature]
  importance_matrix <- importance_matrix[!duplicated(importance_matrix[,'Feature']),]
  importance_matrix <- importance_matrix[order(-abs(Importance))]
  if (!is.null(top_n)) {
    top_n <- min(top_n, nrow(importance_matrix))
    importance_matrix <- head(importance_matrix, top_n)
  }
  if (rel_to_first) {
    importance_matrix[, `:=`(Importance, Importance/max(abs(Importance)))]
  }
  if (is.null(cex)) {
    cex <- 2.5/log2(1 + nrow(importance_matrix))
  }
  if (plot) {
    op <- par(no.readonly = TRUE)
    mar <- op$mar
    if (!is.null(left_margin))
      mar[2] <- left_margin
    par(mar = mar)
    importance_matrix[nrow(importance_matrix):1, barplot(Importance,
                                                         horiz = T, border = T, cex.names = cex, names.arg = Feature,
                                                         las = 1, ...)]
    grid(NULL, NA)
    importance_matrix[nrow(importance_matrix):1, barplot(Importance,
                                                         horiz = T, border = T, add = T, ...)]
    par(op)
  }
  invisible(importance_matrix)
}

#Codigo del grafico de importancia de variables
xgb.varImp <- function(booster = "gbtree"){
  paste0("nombres <- modelo.xgb.",booster,"$feature_names\n",
         "variables.importantes <- xgb.importance(feature_names = nombres, model = modelo.xgb.",booster,")\n",
         "xgb.plot.importance(importance_matrix = variables.importantes, col = ",as.string.c(gg_color_hue(ncol(datos.aprendizaje) - 1)),")")
}

# Pagina de RL --------------------------------------------------------------------------------------------------------------


#Crea el modelo RL
rl.modelo <- function(){
  return(paste0("modelo.rl <<- glm(",variable.predecir,"~., data = datos.aprendizaje, family = binomial)"))
}

rl.modelo.np <- function(){
  return(paste0("modelo.nuevos <<- glm(",variable.predecir.pn,"~., data = datos.aprendizaje.completos, family = binomial)"))
}

#Codigo de la prediccion de rl
rl.prediccion <- function() {
  labels <- rownames(contrasts(datos[,variable.predecir]))
  return(paste0("prediccion.rl <<- predict(modelo.rl, datos.prueba, type = 'response')\n",
                "prediccion.rl <<- ifelse(prediccion.rl > 0.5,'",labels[2],"','",labels[1],"')"))
}

rl.prediccion.np <- function() {
  labels <- rownames(contrasts(datos.aprendizaje.completos[,variable.predecir.pn]))
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos, type = 'response')\n",
                "predic.nuevos <<- ifelse(predic.nuevos > 0.5,'",labels[2],"','",labels[1],"')"))
}

#Codigo de la matriz de confucion de rl
rl.MC <- function(){
  return(paste0("real <- datos.prueba$",variable.predecir,"\n",
                "prediccion <- prediccion.rl\n",
                "MC.rl <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n"))
}

# Pagina de RLR -------------------------------------------------------------------------------------------------------------

rlr.type <- function(){
  ifelse(input$alpha.rlr == 0, "ridge", "lasso")
}

#Crea el modelo RL
rlr.modelo <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
                "y <- datos.aprendizaje[, '",variable.pr,"']\n",
                "modelo.rlr.",rlr.type()," <<- glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')"))
}

rlr.modelo.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2){
  landa <- ifelse(manual,"",paste0("cv.glm.nuevos <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')\n"))
  return(paste0("x <- model.matrix(",variable.predecir.pn,"~., datos.aprendizaje.completos)[, -1]\n",
                "y <- datos.aprendizaje.completos[, '",variable.predecir.pn,"']\n",
                landa,
                "modelo.nuevos <<- glmnet(x, y,standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')"))
}

select.landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",rlr.type()," <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,",family = 'multinomial')")
}

plot.coeff.landa <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",rlr.type(),", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}

#Codigo de la prediccion de rlr
rlr.prediccion <- function(variable.pr = NULL,landa = NULL) {
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("prueba <- model.matrix(",variable.pr,"~., datos.prueba)[, -1]\n",
         "prediccion.rlr.",rlr.type()," <<- predict(modelo.rlr.",rlr.type(),", prueba,",
         "s = ",landa,", type='class')")
}

rlr.prediccion.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2) {
  landa <- ifelse(manual, landa, "cv.glm.nuevos$lambda.min")
  paste0("dp <- datos.prueba.completos\n",
         "dp[, '",variable.predecir.pn,"'] <- 0\n",
         "prueba <- model.matrix(",variable.predecir.pn,"~., dp)[, -1]\n",
         "predic.nuevos <<- predict(modelo.nuevos, prueba,",
         "s = ",landa,", type='class')")
}

#Codigo de la matriz de confucion de rlr
rlr.MC <- function(){
  return(paste0("real <- datos.prueba$",variable.predecir,"\n",
                "prediccion <- prediccion.rlr.",rlr.type(),"\n",
                "MC.rlr.",rlr.type()," <<- crear.matriz.conf(real, prediccion,",num.categorias.pred(),")\n"))
}

# Pagina de TABLA COMPARATIVA -----------------------------------------------------------------------------------------------

#Hace el grafico de una curba de roc
plotROCInd <- function(prediccion,real,adicionar=FALSE,color="red") {
  pred <- ROCR::prediction(prediccion,real)
  perf <- ROCR::performance(pred,"tpr","fpr")
  plot(perf, col=color, add=adicionar, main="Curva ROC")
  segments(0,0,1,1, col='black')
  grid()
}

#Hace el grafico de la curba de roc de los modelos
plotROC <- function(sel) {
  clase <- datos.prueba[,variable.predecir]
  col <- gg_color_hue(length(scores))
  nombres <- c()
  colores <- c()
  adicionar <- FALSE
  index <- 1

  nombres.tr <- unlist(lapply(names(scores), split_name))
  SCORES <- scores[nombres.tr %in% sel]
  nombres.tr <- nombres.tr[nombres.tr %in% sel]

  if(length(SCORES) == 0) {
    return(NULL)
  }

  correcion.xgb <- names(SCORES)[grepl("xgb", names(SCORES))]

  for (i in correcion.xgb) {
    SCORES[[i]] <- data.frame(1-SCORES[[i]],SCORES[[i]])
    colnames(SCORES[[i]]) <- levels(clase)
    SCORES[[i]] <- as.matrix(SCORES[[i]])
  }

  if(any("rl" %in% names(SCORES))){
    SCORES[["rl"]] <- data.frame(1-SCORES[["rl"]],SCORES[["rl"]])
    colnames(SCORES[["rl"]]) <- levels(clase)
    SCORES[["rl"]] <- as.matrix(SCORES[["rl"]])
  }

  if(!is.null(SCORES[["nn"]])){
    colnames(SCORES[["nn"]]) <- levels(clase)
  }

  for (nombre in names(SCORES)) {
    if(is.numeric(SCORES[[nombre]])){
      plotROCInd(as.data.frame(SCORES[[nombre]])[,which(levels(clase) == input$roc.sel)],clase, adicionar, col[index])
    }else{
      if(is.factor(SCORES[[nombre]])){
        plotROCInd(attributes(SCORES[[nombre]])$probabilities[,input$roc.sel],clase,adicionar,col[index])
      }
    }
    adicionar <- TRUE
    colores <- c(colores, col[index])
    nombres <- c(nombres, nombres.tr[index])
    index <- index + 1
  }
  legend(x=0.85, y=0.8, legend = nombres, bty = "n", pch=19 ,
         col = colores , text.col = "black", cex=0.7, pt.cex=0.7)
}

#Calcula el area de la curva ROC
areaROC <- function(prediccion,real) {
  pred <- ROCR::prediction(prediccion,real)
  auc <- ROCR::performance(pred,"auc")
  return(attributes(auc)$y.values[[1]])
}

# Pagina de REPORTE ---------------------------------------------------------------------------------------------------------

combinar.nombres <- function(n.modelos, n.modos){
  res <- c()
  for (modo in n.modos) {
    for (modelo in n.modelos) {
      res <- c(res,paste0(modelo,".",modo))
    }
  }
  return(res)
}

#Ordena el reporte
ordenar.reporte <- function(lista){
  nombres <- names(lista)
  orden <- c("carga.datos","na.delete","transformar.datos","segmentar.datos","resumen",
             nombres[grepl("normalidad.", nombres)],
             nombres[grepl("dispersion.", nombres)],
             nombres[grepl("dya.num.", nombres)],
             nombres[grepl("dya.cat.", nombres)],
             "correlacion","poder.pred",
             nombres[grepl("poder.cat.", nombres)],
             "poder.num",nombres[grepl("poder.den.", nombres)],
             combinar.nombres(c("modelo.knn","pred.knn","mc.knn","ind.knn"),
                              c("optimal", "rectangular", "triangular","epanechnikov",
                                "biweight","triweight", "cos","inv","gaussian")),
             "modelo.dt.gini","modelo.dt.graf.gini","pred.dt.gini",
             "mc.dt.gini","ind.dt.gini","modelo.dt.rules.gini",
             "modelo.dt.information","modelo.dt.graf.information","pred.dt.information",
             "mc.dt.information","ind.dt.information","modelo.dt.rules.information",
             "modelo.rf","modelo.rf.error.","modelo.rf.graf",
             "pred.rf","mc.rf","ind.rf",
             nombres[grepl("modelo.rf.rules.", nombres)],
             "modelo.b.discrete","modelo.b.error.discrete","modelo.b.imp.discrete",
             "pred.b.discrete","mc.b.discrete","ind.b.discrete",
             nombres[grepl("modelo.b.rules.discrete.", nombres)],
             "modelo.b.real","modelo.b.error.real","modelo.b.imp.real","pred.b.real",
             "mc.b.real","ind.b.real",
             nombres[grepl("modelo.b.rules.real.", nombres)],
             "modelo.b.gentle","modelo.b.error.gentle","modelo.b.imp.gentle",
             "pred.b.gentle","mc.b.gentle","ind.b.gentle",
             nombres[grepl("modelo.b.rules.gentle.", nombres)],
             "modelo.svm.linear",
             nombres[grepl("svm.plot.linear", nombres)],
             "pred.svm.linear","mc.svm.linear","ind.svm.linear",
             "modelo.svm.polynomial",
             nombres[grepl("svm.plot.polynomial", nombres)],
             "pred.svm.polynomial","mc.svm.polynomial","ind.svm.polynomial",
             "modelo.svm.radial",
             nombres[grepl("svm.plot.radial", nombres)],
             "pred.svm.radial","mc.svm.radial","ind.svm.radial",
             "modelo.svm.sigmoid",
             nombres[grepl("svm.plot.sigmoid", nombres)],
             "pred.svm.sigmoid","mc.svm.sigmoid","ind.svm.sigmoid",
             "modelo.bayes", "pred.bayes", "mc.bayes", "ind.bayes",
             "modelo.nn", "modelo.nn.graf", "pred.nn", "mc.nn", "ind.nn",
             combinar.nombres(c("modelo.xgb", "modelo.xgb.graf", "pred.xgb", "mc.xgb", "ind.xgb"),
                              c("gbtree", "gblinear", "dart")),
             "modelo.rl","pred.rl", "mc.rl","ind.rl",
             combinar.nombres(c("modelo.rlr","posib.landa.rlr", "gcoeff.landa.rlr", "pred.rlr", "mc.rlr", "ind.rlr"),
                              c("ridge", "lasso")),
             "tabla.comparativa", "roc")

  orden <- c(orden, nombres[!(nombres %in% orden)])
  lista <- lista[orden]
  lista <- lista[!as.logical(lapply(lista, is.null))]
  return(lista)
}

def.reporte <- function(titulo = "Sin Titulo", nombre = "PROMiDAT", entradas) {
  codigo.usuario <- ""
  codigos <- env.report$codigo.reporte
  for (lista in codigos) {
    lista <- ordenar.reporte(lista)
    for (codigo in lista) {
      if(!is.data.frame(codigo)){
        codigo.usuario <- paste0(codigo.usuario, codigo)
      }
    }
  }
  paste0(
    "---\n", "title: '", titulo, "'\n", "author: '", nombre, "'\n",
    "date: ", Sys.Date(), "\n", "output:\n  word_document:\n",
    "    df_print: paged\n---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15, error = T)\n",
    "```\n\n",
    "```{r message=FALSE, warning=FALSE}\n",
    "library(promises)\nlibrary(ggplot2)\n",
    "library(corrplot)\nlibrary(scatterplot3d)\n",
    "library(stringr)\n",
    "library(kknn)\nlibrary(e1071)\nlibrary(rpart)\n",
    "library(rpart.plot)\nlibrary(randomForest)\nlibrary(ada)\nlibrary(xgboost)\n",
    "library(dplyr)\nlibrary(forcats)\n",
    "library(xtable)\n",
    "```\n\n", "```{r}\n", extract.code("var.numericas"), "\n\n",
    extract.code("var.categoricas"), "\n\n", extract.code("datos.disyuntivos"),
    "\n\n", extract.code("prettySeq"), "\n\n",
    "\n\n", extract.code("createDataPartition"), "\n\n",
    "\n\n", extract.code("dummy"), "\n\n",
    "\n\n", extract.code("max.col"), "\n\n",
    "\n\n", extract.code("dummy.data.frame"), "\n\n",
    extract.code("distribucion.categorico"), "\n\n```",
    codigo.usuario)
}

recover.cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else if (substring(file, 1L, 1L) == "|") {
        file <- pipe(substring(file, 2L), "w")
        on.exit(close(file))
      }
      else {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
      }
      .Internal(cat(list(...), file, sep, fill, labels, append))
  }

  lockBinding("cat",.BaseNamespaceEnv)
}

overwrite.cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    file <- stderr()
    sep <- ""

    msg <- .makeMessage(..., domain = NULL, appendLF = TRUE)
    call <- sys.call()
    cond <- simpleMessage(msg, call)

    if (is.character(file))
      if (file == "")
        file <- stdout()
    else if (substring(file, 1L, 1L) == "|") {
      file <- pipe(substring(file, 2L), "w")
      on.exit(close(file))
    }
    else {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    defaultHandler <- function(c) {
      base:::.Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
    }
    withRestarts({
      signalCondition(cond)
      defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
  }

  lockBinding("cat",.BaseNamespaceEnv)
}


# VARIABLES GLOBALES --------------------------------------------------------------------------------------------------------

# -------------------  Datos
datos <<- NULL
datos.originales <<- NULL
datos.prueba <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL
contador <<- 0
semilla <<- FALSE

nombres.modelos <<- c()

# -------------------  Estadisticas Basicas

correlacion <<- NULL
cod.poder.cat <- NULL
cod.poder.num <- NULL
#cod.disp <- default.disp()
#cod.cor <- correlaciones()
#cod.dya.cat <- def.code.cat()
#cod.dya.num <- def.code.num()

# -------------------  Modelos

IndicesM <- list()
areas <- list()
scores <- list()

# -------------------  KNN

cod.knn.modelo <<-  NULL
cod.knn.pred <<-  NULL
cod.knn.mc <<- NULL
cod.knn.ind <<- NULL

knn.stop.excu <<- FALSE

# -------------------  SVM

cod.svm.modelo <<-  NULL
cod.svm.pred <<-  NULL
cod.svm.mc <<- NULL
cod.svm.ind <<- NULL

# -------------------  DT

cod.dt.modelo <<-  NULL
cod.dt.pred <<-  NULL
cod.dt.mc <<- NULL
cod.dt.ind <<- NULL

# -------------------  RF

cod.rf.modelo <<-  NULL
cod.rf.pred <<-  NULL
cod.rf.mc <<- NULL
cod.rf.ind <<- NULL

rf.stop.excu <<- FALSE

# -------------------  BOOSTING

cod.b.modelo <<-  NULL
cod.b.pred <<-  NULL
cod.b.mc <<- NULL
cod.b.ind <<- NULL

# -------------------  BAYES

cod.bayes.modelo <<-  NULL
cod.bayes.pred <<-  NULL
cod.bayes.mc <<- NULL
cod.bayes.ind <<- NULL

# -------------------  NN

cod.nn.modelo <<-  NULL
cod.nn.pred <<-  NULL
cod.nn.mc <<- NULL
cod.nn.ind <<- NULL

NN_EXECUTION <<- TRUE

# -------------------  GX BOOSTING

cod.xgb.modelo <<-  NULL
cod.xgb.pred <<-  NULL
cod.xgb.mc <<- NULL
cod.xgb.ind <<- NULL

# -------------------  Prediccion Nuevos

datos.originales.completos <<- NULL
datos.aprendizaje.completos <<- NULL
datos.prueba.completos <<- NULL

variable.predecir.pn <<- NULL
modelo.seleccionado.pn <<- NULL
contadorPN <<- 0
code.trans.pn <<- ""

modelo.nuevos <<- NULL
predic.nuevos <<- NULL

# -------------------  Reporte

env.report <<- new.env()
env.report$codigo.reporte <- list()
cod.report <<- ""

if(toupper(.Platform$OS.type) != "WINDOWS"){
  enc <<- "utf8"
}else{
  enc <<- "UTF-8"
}
