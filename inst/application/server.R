
shinyServer(function(input, output, session) {

  # FUNCIONES UTILITARIAS ---------------------------------------------------------------------------------------------------

  # Crea una tabla dependiendo de los datos ingresados
  renderizar.tabla.datos <- function(data, editable = TRUE, dom = "frtip", pageLength = 10, scrollY = "27vh") {
    labelsNC <- ifelse(input$idioma == c("es", "es"), c("Numérico","Categórico"), c("Numerical","Categorical"))
    data <- head(data, n = 100)
    nombre.columnas <- c("ID", colnames(data))
    tipo.columnas <- sapply(colnames(data), function(i) ifelse(class(data[,i]) %in% c("numeric", "integer"),
                                                                paste0("<span data-id='numerico'>", labelsNC[1], "</span>"),
                                                                paste0("<span data-id='categorico'>", labelsNC[2], "</span>")))
    tipo.columnas <- lapply(tipo.columnas, function(i)tags$th(HTML(i)))
    sketch <- htmltools::withTags(table(tableHeader(nombre.columnas),
                                        tags$tfoot(tags$tr(tags$th(), tipo.columnas))))

    return(DT::datatable(data, selection = 'none', editable = editable,  container = sketch,
                  options = list(dom = dom, pageLength = pageLength, scrollY = scrollY)))
  }

  # Acualiza las distintas tablas
  actualizar.tabla <- function(x = c("datos", "datos.aprendizaje", "datos.prueba")){
    if(any("datos" %in% x)){ # Cambia la tabla de datos
      output$contents <- DT::renderDT(renderizar.tabla.datos(datos,editable = T), server=F)
    }
    if(any("datos.aprendizaje" %in% x)){ # Cambia la tabla de datos de aprendizaje
      output$contentsAprend <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje,editable=T,scrollY="15vh"),server=F)
    }
    if(any("datos.prueba" %in% x)){ # Cambia la tabla de datos de prueba
      output$contentsPrueba <- DT::renderDT(renderizar.tabla.datos(datos.prueba,editable = T,scrollY="15vh"),server=F)
    }
  }

  # Cierra un menu segun su tabName
  close.menu <- function(tabname = NA, valor = T) {
    select <- paste0("a[href^='#shiny-tab-", tabname, "']")
    if(valor){
      shinyjs::hide(selector = "ul.menu-open")
      shinyjs::disable(selector = select)
    } else {
      shinyjs::enable(selector = select)
    }
  }

  # Validacion comun para todos los modelos
  validar.datos <- function(print = TRUE) {
    # Validaciones
    if (is.null(variable.predecir) & print) {
      showNotification(tr("tieneVP"), duration = 10, type = "error")
    }
    if (is.null(datos) & print) {
      showNotification(tr("tieneD"), duration = 10, type = "error")
    }
    if (is.null(datos.aprendizaje) & print) {
      showNotification(tr("tieneDAP"), duration = 10, type = "error")
    }
    return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
  }

  # Crea la tabla de comparacion entre prediccion y datos reales (datos de prueba)
  obj.predic <- function(predic.var = NULL){
    real <- datos.prueba[, variable.predecir]
    if(is.numeric(predic.var)) {
      predic.var <- factor(predic.var, labels = levels(real))
    }
    real <- as.character(real)
    predi <- as.character(predic.var)
    acerto <- paste0("<span style='color:green'><b>",tr("acerto"),"</b></span>")
    fallo  <- paste0("<span style='color:red'><b>",tr("fallo"),"</b></span>")
    df <- cbind(real, predi, ifelse(real == predi,
                                    rep(acerto, length(real)),
                                    rep(fallo, length(real)) ))
    colns <- c(tr("reald"), tr("pred"), " ")
    colnames(df) <- colns
    sketch <- htmltools::withTags(table(tableHeader(colns)))
    return(DT::datatable(df,
                         selection = "none",
                         editable = FALSE,
                         escape = FALSE,
                         container = sketch,
                         options = list(dom = "frtip", pageLength = 10)))
  }

  # Grafica un error de datos faltantes
  error.variables <- function(num = T) {
    if(num){
      error.plot(tr("errornum"))
    } else {
      error.plot(tr("errorcat"))
    }
  }

  error.plot <- function(msg) {
    res <- ggplot(data.frame(x = c(2, 2.5, 3), y = c(2 ,3 ,2))) +
      geom_polygon(mapping=aes(x=x, y=y), col="gold", fill="gold", alpha=0.3) +
      annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.4, ymax = 2.8) +
      annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.25, ymax = 2.35) +
      annotate("text", x = 2.5, y = 2.1, label = paste0("bold('", msg, "')"),
               size = 8, parse = T) +
      theme(
        panel.background = element_rect(fill = "transparent"),
        axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank()
      )
    return(res)
  }

  #Elimina NULLs
  dropNulls <- function (x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }

  # translates text into current language
  tr <- function(text) {
    sapply(text, function(s) {
      elem <- ifelse(is.null(translation[[s]][[input$idioma]]), s, translation[[s]][[input$idioma]])
      Encoding(elem) <- enc
      elem
    }, USE.NAMES = F)
  }

  updateLabelInput <- function (session, labelid, value = NULL) {
    message <- dropNulls(list(labelid = labelid))
    if(length(labelid) == 1) {
      labelid <- list(labelid)
    }
    ifelse(is.null(value), sentvalue <- tr(labelid),
           ifelse(length(value) == 1, sentvalue <- list(value), sentvalue <- value))
    session$sendCustomMessage(type = 'updateLabel',
                              message = list(ids = labelid, values = sentvalue))
  }

  actualizar.idioma <- function(){
    isolate(updateLabelInput(session, c("idioma","selidioma","data","basico","resumen","normalidad",
                                "dispersion","distribucion","correlacion","poderpred","reporte",
                                "aprendizaje","acercade","comparacion","predicnuevos","knnl","dtl",
                                "rfl","bl","svml","cargar","header","Rownames","eliminana","si","no",
                                "cargarchivo","subir","trans","aplicar","separador","coma","puntocoma",
                                "tab","separadordec","punto","subir","configuraciones","semilla",
                                "habilitada","deshabilitada","seleccionarPredecir","propA","propP",
                                "generar","descargar","dataA","dataP","numerico","categorico","disyuntivo",
                                "resumenvar","selvar","plotnormal","opciones", "selcolor","selvars",
                                "selcolores","codigo","codedist","numericas","categoricas","ejecutar",
                                "selmetodo","seltipo","resultados","distpred","distpredcat","pares",
                                "denspred","generatem","predm","mc","indices","gclasificacion","garbol",
                                "reglas","evolerror","varImp","selkernel","kmax","escal","minsplit",
                                "maxdepth","splitIndex","numTree","numVars","ruleNumTree","selectAlg",
                                "rocCurva","tablaComp","selectMod","selectCat", "reporte","titulo",
                                "nombre","codreporte","salida","copyright","info","version","cargarNuev",
                                "cargarDatos","transDatos","seleParModel","generarM","variables","tipo",
                                "activa","nn","xgb","selbooster","selnrounds","selectCapas","threshold",
                                "stepmax","redPlot","rl","rlr","posibLanda","coeff","gcoeff","automatico",
                                "landa","eliminar", "imputar")))
  }

  # CONFIGURACIONES INICIALES -----------------------------------------------------------------------------------------------



  source("global.R", local = T)
  source("utils.R", local = T)
  load("www/translation.bin")

  #actualizar.idioma()

  options(shiny.maxRequestSize = 200 * 1024^2,
          width = 200,
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
                            scrollX = TRUE, language = list(search = HTML('<i class="fa fa-search"></i>'),
                                                            info = "", emptyTable = "", zeroRecords = "",
                                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))

  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-poderPred]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')

  actualizar.tabla()

  updateAceEditor(session, "fieldCodeResum", value = cod.resum())
  updateAceEditor(session, "fieldModelCor", value = modelo.cor())
  updateAceEditor(session, "fieldFuncNum", extract.code("distribucion.numerico"))
  updateAceEditor(session, "fieldFuncCat", extract.code("distribucion.categorico"))

  # VALORES REACTIVOS -------------------------------------------------------------------------------------------------------

  updatePlot <- reactiveValues(calc.normal = default.calc.normal(), normal = NULL, disp = NULL,
                               cor = NULL, dya.num = NULL, dya.cat = NULL, poder.pred = NULL,
                               poder.cat = NULL, poder.num = NULL, poder.dens = NULL, roc = FALSE,
                               svm.graf = NULL)

  disp.ranges <- reactiveValues(x = NULL, y = NULL)

  # PAGINA DE CARGAR Y TRANSFORMAR DATOS ------------------------------------------------------------------------------------

  # Carga datos
  cargar.datos <- function(codigo.carga = "") {
    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos) <= 1) {
        showNotification(tr("errorCData"), duration = 10, type = "error")
        return(NULL)
      }
      new.report()
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
  }

  # Limpiado datos
  limpiar.datos <- function(){
    if (any(is.na(datos))) {
      tryCatch({
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
       # codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
        isolate(exe(codigo.na))
        insert.report("na.delete",paste0("\n# Imputación de Datos\n```{r}\n",codigo.na,"\nhead(datos)\nstr(datos)\n```"))
      }, error = function(e) {
        showNotification(paste0("Error (NA): ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
    }
    return(codigo.na)
  }

  # Transforma los datos
  transformar.datos <- function() {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if (input[[paste0("box", var, contador)]]) {
        if (input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(exe(code.res))
    code.res <- paste0(code.res, "\n")
    if (length(var.noactivas) > 0) {
      isolate(exe(code.desactivar(var.noactivas)))
      code.res <- paste0(code.res, code.desactivar(var.noactivas))
    }

    new.secction.report()
    insert.report("transformar.datos",paste0("# Transformando Datos\n```{r}\n",code.res,"\nstr(datos)\n```"))
    return(code.res)
  }

  # Actualizar los distintos selectores
  acualizar.selecctores <- function() {
    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = rev(colnames.empty(var.categoricas(datos))))
  }

  # Crea las correlaciones
  ejecutar.modelo.cor <- function() {
    tryCatch({
      exe(text = modelo.cor())
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }

  # Borra los datos de los modelos
  borrar.modelos <- function(flag.datos = TRUE) {
    if (flag.datos) {
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
    }

    areas <<- list()
    scores <<- list()
    IndicesM <<- list()

    actualizar.selector.comparativa()
    rm(list = nombres.modelos, envir = .GlobalEnv)
    nombres.modelos <<- c()

    updateCheckboxGroupButtons(session, inputId = "select.models",
                               choices = c(" ---- " = "NoDisponible"),
                               size = "sm", status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                no = icon("remove", lib = "glyphicon")))

    #Se usan como condicion para el codigo js
    output$txtknn <- renderPrint(invisible(""))
    output$txtSvm <- renderPrint(invisible(""))
    output$txtDt <- renderPrint(invisible(""))
    output$txtRf <- renderPrint(invisible(""))
    output$txtBoosting <- renderPrint(invisible(""))
    output$txtbayes <- renderPrint(invisible(""))
    updateSelectInput(session,"kernel.knn",selected = "optimal")
  }

  # Cunado es precionado el boton de cargar datos
  observeEvent(input$loadButton, {
    codigo.carga <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                               separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

    # Carga los datos
    cargar.datos(codigo.carga)

    # Limpia los datos
    codigo.na <- limpiar.datos()

    # Actualiza el codigo
    if (isTRUE(getOption("shiny.testmode"))) {
      updateAceEditor(session, "fieldCodeData", value = "CODIGO")
    } else {
      updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))
    }


    # Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    # borra los datos de modelos
    borrar.modelos()

    # Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    # Cambia las tablas de datos
    actualizar.tabla()
  }, priority = 4)

  # Cunado es precionado el boton de transformar datos
  observeEvent(input$transButton, {
    # transforma los datos
    code.res <- transformar.datos()

    # Actualiza el codigo
    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    # Actualiza los selectores que dependen de los datos
    acualizar.selecctores()

    # modelo correlacion
    ejecutar.modelo.cor()

    # borra los datos de modelos
    borrar.modelos()

    # Cierra o abre lo s menus los menus
    close.menu("parte1", is.null(datos))
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))

    # Cambia las tablas de datos
    actualizar.tabla()
  }, priority = 4)

  # Crea los select box del panel de trasnformar datos
  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0){
      res <- data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id="sel', i, contador, '"> <option value="categorico">',tr("categorico"),'</option>',
               '<option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric", "integer"),
                                                   ' selected="selected"', ""),'>', tr("numerico"),
               '</option> <option value="disyuntivo">',tr("disyuntivo"),'</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    } else {
      res <- as.data.frame(NULL)
      showNotification(tr("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })

  # Cambia la tabla de con las opciones del panel de transformar
  output$transData <- DT::renderDT({sketch <- htmltools::withTags(table(tags$thead(tags$tr(tags$th(tags$span(`data-id` = "variables", "Variables")),
                                                                                           tags$th(tags$span(`data-id` = "tipo", "Tipo")),
                                                                                           tags$th(tags$span(`data-id` = "activa", "Activa"))))))
                                    DT::datatable(update.trans(),
                                          escape = FALSE, selection = "none", container = sketch,
                                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "45vh"), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))
                                    }, server = FALSE)

  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  # PAGINA DE SEGMENTAR DATOS -----------------------------------------------------------------------------------------------

  # Crea los datos de aprendizaje y prueba
  segmentar.datos <- function(codigo) {
    tryCatch({
      isolate(exe(codigo))
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
    }, error = function(e) {
      showNotification(paste0(tr("errorSeg"), e), duration = 15, type = "error")
    })
  }

  # Actualiza los selecctores relacionados con los datos de prueba y aprendizaje
  acualizar.selecctores.seg <- function() {
    nombres <- colnames.empty(var.numericas(datos))
    updateSelectizeInput(session, "select.var.svm.plot", choices = nombres)
    choices <- as.character(unique(datos[, variable.predecir]))
    updateSelectInput(session, "roc.sel", choices = choices, selected = choices[1])
    cat.sin.pred <- colnames.empty(var.categoricas(datos))
    cat.sin.pred <- cat.sin.pred[cat.sin.pred != input$sel.predic.var]
    updateSelectInput(session, "sel.distribucion.poder", choices = cat.sin.pred)
    updateSelectInput(session, "sel.density.poder", choices = nombres)
    updateAceEditor(session, "fieldCodePoderPred", value = plot.code.poder.pred(variable.predecir))
    updatePlot$poder.pred <<- plot.code.poder.pred(variable.predecir, label = tr("distrelvar"))
  }

  # Segmenta los datos en aprendizaje y prueba
  observeEvent(input$segmentButton, {
    if(input$sel.predic.var != ""){
      codigo <- particion.code("datos", input$segmentacionDatosA,
                               input$sel.predic.var,
                               input$semilla,
                               input$permitir.semilla)

      semilla <<- input$permitir.semilla
      # knn.stop.excu <<- FALSE
      # rf.stop.excu <<- FALSE

      segmentar.datos(codigo)

      new.secction.report()
      insert.report("segmentar.datos",paste0("\n# Datos de Aprendizaje\n```{r}\n",codigo,
                                             "\nhead(datos.aprendizaje)\n```\n\n# Datos de Prueba\n```{r}\nhead(datos.prueba)\n```\n"))

      acualizar.selecctores.seg()

      # borra los datos de modelos
      borrar.modelos(FALSE)

      # Cambia los codigos de los modelos
      default.codigo.knn(k.def = TRUE)
      default.codigo.svm()
      default.codigo.dt()
      deafult.codigo.rf(rf.def = TRUE)
      deault.codigo.boosting()
      default.codigo.bayes()
      default.codigo.nn()
      default.codigo.xgb()
      default.codigo.rl()
      default.codigo.rlr()
    } else {
      showNotification(tr("tieneSVP"), duration = 15, type = "error")
    }

    # Cierre o abre el menu
    close.menu("parte2", is.null(datos.aprendizaje))
    close.menu("comparar", is.null(datos.aprendizaje))
    close.menu("poderPred", is.null(datos.aprendizaje))
    # Cambia las tablas de aprendizaje y de prueba
    actualizar.tabla(c("datos.aprendizaje", "datos.prueba"))
  },priority = 5)

  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.semilla, {
    if (input$permitir.semilla) {
      shinyjs::enable("semilla")
    } else {
      shinyjs::disable("semilla")
    }
  })

  # Cuando cambia la barra de proporcion de datos de prueba (Segmentar Datos)
  observeEvent(input$segmentacionDatosA, {
    updateSliderInput(session, "segmentacionDatosT", value = 100 - input$segmentacionDatosA)
  })

  # Cuando cambia la barra de proporcion de datos de aprendizaje (Segmentar Datos)
  observeEvent(input$segmentacionDatosT, {
    updateSliderInput(session, "segmentacionDatosA", value = 100 - input$segmentacionDatosT)
  })

  output$downloaDatosA <- downloadHandler(
    filename = function(){
      paste0("(",tr("dataA"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.aprendizaje, file, row.names = input$rowname)
    }
  )

  output$downloaDatosP <- downloadHandler(
    filename = function() {
      paste0("(",tr("dataP"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.prueba, file, row.names = input$rowname)
    }
  )

  # PAGINA DE RESUMEN -------------------------------------------------------------------------------------------------------

  # Cambia la tabla con el summary en la pagina de resumen
  output$resumen.completo <- DT::renderDataTable(obj.resum(),
                                                 options = list(dom = "ft", scrollX = TRUE),
                                                 rownames = F)

  # Se crea una tabla summary
  obj.resum <- eventReactive(c(input$loadButton, input$transButton),{
    insert.report("resumen" ,c(paste0("\n## Resumen Numérico \n```{r} \nsummary(datos) \n```")))
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  # Cambia los cuadros de summary por varibale
  output$resumen <- renderUI({
    if (input$sel.resumen %in% colnames(var.numericas(datos))){
      resumen.numerico(datos, input$sel.resumen)
    }else{
      resumen.categorico(datos, input$sel.resumen)
    }
  })

  # PAGINA DEL TEST de Normalidad -------------------------------------------------------------------------------------------

  # Hace el grafico de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
   output$plot.normal <- renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(exe(cod.normal))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        insert.report(paste0("normalidad.", input$sel.normal),paste0("## Test de Normalidad \n```{r}\n", cod.normal, "\n```"))
        return(res)
      }, error = function(e){
        if(ncol(var.numericas(datos)) <= 0){
          error.variables(T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Ejecuta el codigo en el campo del codigo
  # observeEvent(input$run.normal, {
  #   updatePlot$normal <- input$fieldCodeNormal
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal, tr("curvanormal"))
  })

  # Hace la tabla comparativa de la pagina de test de normalidad
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal <- DT::renderDT({
      tryCatch({
        #datos <- updateData$datos
        codigo <- updatePlot$calc.normal
        res <- isolate(exe(codigo))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        fisher <- tr("fisher")
        asimetria <- tr("asimetria")
        sketch = htmltools::withTags(table(
          tags$thead(tags$tr(tags$th(), tags$th(fisher), tags$th(asimetria)))
        ))
        DT::datatable(
          res, selection = 'none', container = sketch,
          options = list(dom = 'frtip', scrollY = "40vh")
        )
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta la tabla comparativa
  # observeEvent(input$run.calc.normal, {
  #   updatePlot$calc.normal <- input$fieldCalcNormal
  # })

  # PAGINA DE Dispersion ----------------------------------------------------------------------------------------------------

  # Hace el grafico de dispersion
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.disp <- renderPlot({
      tryCatch({
        if(ncol(var.numericas(datos)) <= 1){
          error.variables(T)
        } else {
          cod.disp <<- updatePlot$disp
          updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
          if(!is.null(cod.disp) && cod.disp != "") {
            insert.report(paste0("dispersion.", paste(input$select.var, collapse = ".")),
                          paste0("## Dispersión \n```{r}\n", cod.disp, "\n```"))
          }
          return(isolate(exe(cod.disp)))
        }
      }, error = function(e) {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
      })
    })
  })

  #Hace el grafico del zoom
  output$plot.disp.zoom <- renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- isolate(exe(cod.disp))
      res <- res + coord_cartesian(xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })

  #Hace la tabal de con los valores de disperción.
  output$mostrar.disp.zoom <- DT::renderDataTable({
    tryCatch({
      return(brushedPoints(datos[, input$select.var], input$zoom.disp))
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh", pageLength = nrow(datos)))

  # Si se selecciona un area de zoom
  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.disp, {
  #   updatePlot$disp <- input$fieldCodeDisp
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$select.var, input$col.disp), {
    if (length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  # PAGINA DE Distribucion --------------------------------------------------------------------------------------------------

  # Hace el grafico de Distribucion numerico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num <<- updatePlot$dya.num
        res <- isolate(exe(cod.dya.num))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        insert.report(paste0("dya.num.", input$sel.distribucion.num),
                      paste0("## Distribución y atipicidad \n```{r}\n", cod.dya.num,"\n```"))
        return(res)
      }, error = function(e) {
        if (ncol(var.numericas(datos)) == 0){
          error.variables(T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Ejecuta el codigo del grafico numerico
  # observeEvent(input$run.dya.num, {
  #   updatePlot$dya.num <- input$fieldCodeNum
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def.code.num(
      data = "datos", color = paste0("'", input$col.dist, "'"),
      variable = paste0("'", input$sel.distribucion.num, "'")
    )
  })

  tabla.atipicos <- function(){
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
    datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",pageLength = nrow(datos))) %>%
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  }

  # Crea la tabla de atipicos
  observeEvent(c(input$distribucion_numerica), {
    output$mostrar.atipicos <- DT::renderDataTable({tabla.atipicos()})
  })

  # Hace el grafico de Distribucion categorico
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat <<- updatePlot$dya.cat
        res <- isolate(exe(cod.dya.cat))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        insert.report(paste0("dya.cat.", input$sel.distribucion.cat),
                      paste0("## Distribución \n```{r}\n", cod.dya.cat, "\n```"))
        return(res)
      }, error = function(e) {
        if (ncol(var.categoricas(datos)) == 0){
          error.variables(T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Ejecuta el codigo del grafico categorico
  # observeEvent(input$run.dya.cat, {
  #   updatePlot$dya.cat <- input$fieldCodeCat
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def.code.cat(variable = input$sel.distribucion.cat)
  })

  # PAGINA DE Correlacion ---------------------------------------------------------------------------------------------------

  # Hace el grafico de correlacion
  observeEvent(c(input$loadButton, input$transButton),{ #, input$fieldModelCor), {
    output$plot.cor <- renderPlot({
      tryCatch({
        cod.cor <<- updatePlot$cor
        res <- isolate(exe(cod.cor))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        insert.report("correlacion", paste0("## Correlación \n```{r}\n",modelo.cor(),"\n", cod.cor, "\n```"))
        return(res)
      }, error = function(e) {
        if (ncol(var.numericas(datos)) <= 1){
          error.variables(T)
        }else{
          showNotification(paste0("ERROR EN Correlacion: ", e),
                           duration = 10,
                           type = "error")
          return(NULL)
        }
      })
    })
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.code.cor, {
  #   updatePlot$cor <- input$fieldCodeCor
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  # PAGINA DE Poder Predictivo ----------------------------------------------------------------------------------------------

  # Hace el grafico de poder predictivo distribucion de la variable predictora
  observeEvent(input$segmentButton, {
    output$plot.pred.poder <- renderPlot({
      tryCatch({
        cod.poder <<- updatePlot$poder.pred
        res <- isolate(exe(cod.poder))
        updateAceEditor(session, "fieldCodePoderPred", value = cod.poder)
        insert.report("poder.pred", paste0("## Distribución Variable Discriminante \n```{r}\n", cod.poder, "\n```"))
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.code.poder.pred, {
  #   updatePlot$poder.pred <- input$fieldCodePoderPred
  # })

  # Hace el grafico de poder predictivo categorico
  observeEvent(input$segmentButton, {
    output$plot.dist.poder <- renderPlot({
      tryCatch({
        cod.poder.cat <<- updatePlot$poder.cat
        updateAceEditor(session, "fieldCodePoderCat", value = cod.poder.cat)
        if (ncol(var.categoricas(datos)) > 1) {
          res <- isolate(exe(cod.poder.cat))
          insert.report(paste0("poder.cat.",input$sel.distribucion.poder),
                        paste0("## Distribución Según Variable Discriminante \n```{r}\n", cod.poder.cat, "\n```"))
        }else{
          res <- error.variables(F)
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e), duration = 10,type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.code.poder.cat, {
  #   updatePlot$poder.cat <- input$fieldCodePoderCat
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.distribucion.poder, {
    if (input$sel.distribucion.poder != "") {
      updatePlot$poder.cat <- plot.code.dist.porc(input$sel.distribucion.poder, variable.predecir, label=tr("distpodcat"))
    } else {
      updatePlot$poder.cat <- ""
    }
  })

  # Hace el grafico de poder predictivo numerico
  observeEvent(input$segmentButton,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var.numericas(datos)) >= 2) {
          if(ncol(var.numericas(datos)) <= 25){
            res <- isolate(exe(cod.poder.num))
            insert.report("poder.num",paste0("## Poder Predictivo Variables Numéricas \n```{r}\n", cod.poder.num, "\n```"))
            return(res)
          }else{
            showNotification(tr("bigPlot"), duration = 10, type = "message")
            return(NULL)
          }
        }else{
          res <- error.variables(T)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.code.poder.num, {
  #   if(input$fieldCodePoderNum != "") {
  #     updatePlot$poder.num <- input$fieldCodePoderNum
  #   } else {
  #     updatePlot$poder.num <- pairs.poder()
  #   }
  # })

  observeEvent(input$segmentButton,{
    updatePlot$poder.num <- pairs.poder()
  }, priority = 3)

  # Hace el grafico de poder predictivo densidad de variables numericas
  observeEvent(input$segmentButton, {
    output$plot.density.poder <- renderPlot({
      tryCatch({
        cod.poder.den <<- updatePlot$poder.dens
        updateAceEditor(session, "fieldCodePoderDens", value = cod.poder.den)
        if (ncol(var.numericas(datos)) >= 1) {
          res <- isolate(exe(cod.poder.den))
          insert.report(paste0("poder.den.",input$sel.density.poder),
                        paste0("## Densidad Según Variable Discriminante\n```{r}\n", cod.poder.den, "\n```"))
        }else{
          res <- error.variables(T)
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Ejecuta el codigo del grafico
  # observeEvent(input$run.code.poder.dens,{
  #   updatePlot$poder.dens <- input$fieldCodePoderDens
  # })

  # Ejecuta el codigo cuando cambian los parametros
  observeEvent(input$sel.density.poder, {
    if (input$sel.density.poder != "") {
      updatePlot$poder.dens <- plot.numerico.dens(input$sel.density.poder,tr("denspodlab"))
    } else {
      updatePlot$poder.dens <- ""
    }
  })

  # Pagina DE KNN -----------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo knn
  observeEvent(input$runKnn, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      knn.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$switch.scale.knn, input$kmax.knn, input$kernel.knn), {
    if (validar.datos(print = FALSE) & knn.stop.excu) {
      default.codigo.knn()
    }else{
      knn.stop.excu <<- TRUE
    }
  },priority = 5)

  # Acualiza el codigo a la version por defecto
  default.codigo.knn <- function(k.def = FALSE) {
    if(!is.null(datos.aprendizaje) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(datos.aprendizaje))), input$kmax.knn)
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- input$kmax.knn
    }

    kernel <- isolate(input$kernel.knn)

    # Se acualiza el codigo del modelo
    codigo <- kkn.modelo(
      variable.pr = variable.predecir,
      scale = input$switch.scale.knn,
      kmax = k.value,
      kernel = kernel
    )
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- kkn.prediccion(kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- knn.MC(variable.predecir, kernel = kernel)
    updateAceEditor(session, "fieldCodeKnnMC", value = codigo)
    cod.knn.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.knn <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.knn.",input$kernel.knn," <<- NULL")
        output$txtknn <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.knn.",input$kernel.knn))
      }, {
        exe("prediccion.knn.",input$kernel.knn," <<- NULL")
        remove.report.elem(paste0("pred.knn.",input$kernel.knn))
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.knn.",input$kernel.knn," <<- NULL")
        remove.report.elem(paste0("mc.knn.",input$kernel.knn))
        output$plot.knn.mc <- renderPlot(NULL)
        output$txtknnMC <- renderPrint(invisible(NULL))
      }, {
        IndicesM[[paste0("knnl-",input$kernel.knn)]] <<- NULL
        exe("indices.knn.",input$kernel.knn," <<- NULL")
        remove.report.elem(paste0("ind.knn.",input$kernel.knn))
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de knn
  knn.full <- function() {
    ejecutar.knn()
    ejecutar.knn.pred()
    ejecutar.knn.mc()
    ejecutar.knn.ind()
  }

  # Genera el modelo
  ejecutar.knn <- function() {
    tryCatch({
      exe(cod.knn.modelo)
      kernel <- isolate(input$kernel.knn)
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn <- renderPrint(exe("modelo.knn.",kernel))
      insert.report(paste0("modelo.knn.",kernel),
                    paste0("## Generación del modelo KNN - ",kernel,"\n```{r}\n",cod.knn.modelo, "\nmodelo.knn.",kernel,"\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.knn.",kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(1)
      showNotification(paste0("Error (KNN-01) : ", e), duration = 15, type = "error")
    }
    )
  }

  # Genera la prediccion
  ejecutar.knn.pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.knn.pred)
      kernel <- isolate(input$kernel.knn)
      scores[[paste0("knnl-",kernel)]] <<- predict(exe("modelo.knn.",kernel), datos.prueba, type = "prob")

      # Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.knn.",kernel)),server = FALSE)
      insert.report(paste0("pred.knn.",kernel),
                    paste0("## Predicción del Modelo KNN - ",kernel,"\n```{r}\n", cod.knn.pred,
                           "\nscores[['",paste0("knnl-",kernel),"']] <<- predict(modelo.knn.",kernel,",datos.prueba, type = 'prob')\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.knn.",kernel))
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.knn.mc <- function() {
    kernel <- isolate(input$kernel.knn)
    if(exists(paste0("prediccion.knn.",kernel))){
      tryCatch({ # Se corren los codigo
        exe(cod.knn.mc)
        output$txtknnMC <- renderPrint(print(exe("MC.knn.",kernel)))

        exe(plot.MC.code())
        output$plot.knn.mc <- renderPlot(exe("plot.MC(MC.knn.",kernel,")"))
        insert.report(paste0("mc.knn.",kernel),
                      paste0("## Matriz de confusión del Modelo KNN - ",
                             kernel,"\n```{r}\n", cod.knn.mc,
                             "\nMC.knn.",kernel,
                             "\n```\n```{r}\nplot.MC(MC.knn.",kernel,")\n```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.knn.",kernel))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.knn(3)
        showNotification(paste0("Error (KNN-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.knn.ind <- function(){
    kernel <- isolate(input$kernel.knn)
    if(exists(paste0("MC.knn.",kernel))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.knn.ind))

        indices.knn <- indices.generales(exe("MC.knn.",kernel))
        eval(parse(text = paste0("indices.knn.",kernel, "<<- indices.knn")))
        #indices.g("knn", MC)

        insert.report(paste0("ind.knn.",kernel),
                      paste0("## Índices Generales del Modelo KNN - ",kernel,"\n```{r}\n",
                             cod.knn.ind, "\nindices.generales(MC.knn.",kernel,")\n",
                             "IndicesM[['knnl-",kernel,"']] <<- indices.generales(MC.knn.",kernel,")\n```"))

        nombres <- c("knnPrecGlob", "knnErrorGlob")
        fill.gauges(nombres, indices.knn)

        # Cambia la tabla con la indices de knn
        output$knnIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.knn,"KNN")), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)

        output$knnIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.knn,"KNN")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.knn.",kernel))
        IndicesM[[paste0('knnl-',kernel)]] <<- indices.knn
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.knn(4)
        showNotification(paste0("Error (KNN-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE SVM -----------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo svm
  observeEvent(input$runSvm, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      svm.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$switch.scale.svm, input$kernel.svm), {
    if (validar.datos(print = FALSE)){
      default.codigo.svm()
    }
  }, priority =  5)

  # Acualiza el codigo a la version por defecto
  default.codigo.svm <- function() {
    kernel <- isolate(input$kernel.svm)
    # Se acualiza el codigo del modelo
    codigo <- svm.modelo(variable.pr = variable.predecir,
                         scale = input$switch.scale.svm,
                         kernel = kernel)

    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo

    # # Acutaliza el codigo del grafico de clasificacion svm
    # updateAceEditor(session, "fieldCodeSvmPlot", value = "")

    # Se genera el codigo de la prediccion
    codigo <- svm.prediccion(kernel)
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- svm.MC(variable.predecir,kernel)
    updateAceEditor(session, "fieldCodeSvmMC", value = codigo)
    cod.svm.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.svm <- function(capa = NULL){
    for(i in capa:4){
      switch(i, {
        exe("modelo.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvm <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.svm.",input$kernel.svm))
        remove.report.elem(grepl(paste0("svm.plot.",input$kernel.svm), names.report()))
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        remove.report.elem(paste0("pred.svm.",input$kernel.svm))
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.svm.",input$kernel.svm,"<<- NULL")
        remove.report.elem(paste0("mc.svm.",input$kernel.svm))
        output$txtSvmMC <- renderPrint(invisible(""))
        output$plot.svm.mc <- renderPlot(NULL)
      }, {
        IndicesM[[paste0("svml-",input$kernel.svm)]] <<- NULL
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
        remove.report.elem(paste0("ind.svm.",input$kernel.svm))
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de svm
  svm.full <- function() {
    ejecutar.svm()
    ejecutar.svm.pred()
    ejecutar.svm.mc()
    ejecutar.svm.ind()
  }

  # Genera el modelo
  ejecutar.svm <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.modelo))
      kernel <- isolate(input$kernel.svm)
      output$txtSvm <- renderPrint(exe("print(modelo.svm.",kernel,")"))
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)

      insert.report(paste0("modelo.svm.",kernel),
                    paste0("## Generación del modelo SVM - ",kernel,"\n```{r}\n",
                           cod.svm.modelo, "\nmodelo.svm.",kernel,"\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.",kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.svm(1)
      showNotification(paste0("Error (SVM-01) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.svm.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.pred))
      kernel <- isolate(input$kernel.svm)

      modelo.svm.roc <- svm(as.formula(paste0(variable.predecir, "~.")),
                            data = datos.aprendizaje,
                            scale = T,
                            kernel = kernel,
                            probability = T)

      scores[[paste0("svml-",kernel)]] <<- predict(modelo.svm.roc, datos.prueba, probability = T)

      # Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(exe("obj.predic(prediccion.svm.",kernel,")"),server = FALSE)
      insert.report(paste0("pred.svm.",kernel),
                    paste0("## Predicción del Modelo SVM - ",kernel,"\n```{r}\n", cod.svm.pred,
                           "\nmodelo.svm.roc <- svm(as.formula(paste0(variable.predecir, '~.')),data = datos.aprendizaje,scale = T,kernel = '",
                           kernel,"',probability = T)\n",
                           "scores[['svml-",kernel,"']] <<- predict(modelo.svm.roc,datos.prueba, probability = T)\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))

      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.svm.mc <- function(){
    kernel <- isolate(input$kernel.svm)
    if(exists(paste0("prediccion.svm.",kernel))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.mc))
        output$txtSvmMC <- renderPrint(exe("print(MC.svm.",kernel,")"))
        isolate(exe(plot.MC.code()))
        output$plot.svm.mc <- renderPlot(exe("plot.MC(MC.svm.",kernel,")"))

        insert.report(paste0("mc.svm.",kernel),
                      paste0("## Matriz de Confusión del Modelo SVM - ",kernel,"\n```{r}\n",
                             cod.svm.mc, "\nMC.svm.",kernel,"\n```\n```{r}\nplot.MC(MC.svm.",kernel,")\n```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.svm.",kernel))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.svm(3)
        showNotification(paste0("Error (SVM-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.svm.ind <- function(){
    kernel <- isolate(input$kernel.svm)
    if(exists(paste0("MC.svm.",kernel))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.ind))

        MC <- exe("MC.svm.",kernel)
        indices.svm <- indices.generales(MC)
        eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))

        insert.report(paste0("ind.svm.",kernel),
                      paste0("## Índices Generales del modelo SVM  - ",kernel," \n```{r}\n",
                             cod.svm.ind, "\nindices.generales(MC.svm.",kernel,")\n",
                             "IndicesM[['svml-",kernel,"']] <<- indices.generales(MC.svm.",kernel,")\n```"))

        nombres <- c("svmPrecGlob", "svmErrorGlob")
        fill.gauges(nombres, indices.svm)

        # Cambia la tabla con la indices de svm
        output$svmIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.svm,"SVM")), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)

        output$svmIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.svm,"SVM")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.svm.",kernel))
        IndicesM[[paste0("svml-",kernel)]] <<- indices.svm
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.knn(4)
        showNotification(paste0("Error (SVM-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  #Hace el grafico de svm
  output$plot.svm <- renderPlot({
    tryCatch({
      codigo <- updatePlot$svm.graf
      if(!is.null(codigo) & codigo != ""){
        insert.report(paste0("svm.plot.",input$kernel.svm,paste0(input$select.var.svm.plot, collapse = ".")),
                      paste0("\n```{r}\n",codigo, "\n```"))
        exe(codigo)
      }else{
        if(!(ncol(var.numericas(datos)) >= 2)){
          error.variables(T)
        }else{
          return(NULL)
        }
      }},error = function(e){
        return(NULL)
    })
  })

  # Cuando cambia el codigo del grafico de clasificacion svm
  observeEvent(c(input$runSvm, input$select.var.svm.plot),{
    if (length(input$select.var.svm.plot) == 2){
      v <- colnames(datos)
      v <- v[v != variable.predecir]
      v <- v[!(v %in% input$select.var.svm.plot)]
      if(length(v) == 0){
        v <- input$select.var.svm.plot
      }
      isolate(kernel <- input$kernel.svm)
      updateAceEditor(session, "fieldCodeSvmPlot", value = svm.plot(input$select.var.svm.plot, v, kernel))
    }else{
      updatePlot$svm.graf <- NULL
    }
  })

  observeEvent(c(input$fieldCodeSvmPlot),{
     updatePlot$svm.graf <- input$fieldCodeSvmPlot
  })

  # PAGINA DE DT ------------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo dt
  observeEvent(input$runDt, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      dt.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$minsplit.dt, input$maxdepth.dt, input$split.dt), {
    if (validar.datos(print = FALSE)){
      default.codigo.dt()
    }
  }, priority =  5)

  # Acualiza el codigo a la version por defecto
  default.codigo.dt <- function() {

    tipo <- isolate(input$split.dt)

    # Se acualiza el codigo del modelo
    codigo <- dt.modelo(variable.pr = variable.predecir,
                        minsplit = input$minsplit.dt,
                        maxdepth = input$maxdepth.dt,
                        split = tipo)

    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo

    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())

    # Se genera el codigo de la prediccion
    codigo <- dt.prediccion()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- dt.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeDtMC", value = codigo)
    cod.dt.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }

  #Plotear el arbol
  plotear.arbol <- function(){
    tryCatch({
      output$plot.dt <- renderPlot(isolate(exe(input$fieldCodeDtPlot)))
      cod <- ifelse(input$fieldCodeDtPlot == "", dt.plot(), input$fieldCodeDtPlot)
      insert.report(paste0("modelo.dt.graf.", input$split.dt), paste0("\n```{r}\n", cod, "\n```"))
    },
    error = function(e){
      output$plot.dt <- renderPlot(NULL)
      insert.report(paste0("modelo.dt.graf.",input$split.dt),NULL)
    })
  }

  #Mostrar Reglas
  mostrar.reglas.dt <- function(){
    tipo <- isolate(input$split.dt)
    output$rulesDt <- renderPrint(rattle::asRules(exe("modelo.dt.",tipo)))
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt.",tipo,")"))
    insert.report(paste0("modelo.dt.rules.",tipo),
                  paste0("\n# Reglas del Árbol\n```{r}\nrattle::asRules(modelo.dt.",tipo,")\n```"))
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.dt <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.dt <<- NULL
        output$txtDt <- renderPrint(invisible(""))
        output$plot.dt <- renderPlot(NULL)
        remove.report.elem(paste0("modelo.dt.", input$split.dt))
        remove.report.elem(paste0("modelo.dt.graf.", input$split.dt))
      }, {
        prediccion.dt <<- NULL
        remove.report.elem(paste0("pred.dt.", input$split.dt))
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.dt <<- NULL
        remove.report.elem(paste0("mc.dt.", input$split.dt))
        output$plot.dt.mc <- renderPlot(NULL)
        output$txtDtMC <- renderPrint(invisible(NULL))
      }, {
        indices.dt <<- rep(0, 10)
        remove.report.elem(paste0("ind.dt.",input$split.dt))
        IndicesM[[paste0("dtl-",input$split.dt)]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de dt
  dt.full <- function() {
    ejecutar.dt()
    ejecutar.dt.pred()
    ejecutar.dt.mc()
    ejecutar.dt.ind()
  }

  # Genera el modelo
  ejecutar.dt <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.modelo))

      tipo <- isolate(input$split.dt)
      output$txtDt <- renderPrint(print(exe("modelo.dt.", tipo)))
      insert.report(paste0("modelo.dt.", tipo),
                    paste0("## Generación del modelo Árboles de Decisión\n```{r}\n", cod.dt.modelo,
                           "\nmodelo.dt.",tipo,"\n```"))
      plotear.arbol()
      mostrar.reglas.dt()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.dt.", tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(1)
      showNotification(paste0("Error (DT-01) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.dt.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.pred))
      tipo <- isolate(input$split.dt)

      scores[[paste0("dtl-", tipo)]] <<- exe("predict(modelo.dt.",tipo,", datos.prueba, type = 'prob')")
      # Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.dt.",tipo)),server = FALSE)

      insert.report(paste0("pred.dt.", tipo),
                    paste0("## Predicción del Modelo Árboles de Decisión\n```{r}\n", cod.dt.pred,
                           "\nscores[['dtl-",tipo,"']] <<- predict(modelo.dt.",tipo,", datos.prueba, type = 'prob')\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.dt.",tipo))
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.dt(2)
      showNotification(paste0("Error (DT-02) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.dt.mc <- function() {
    tipo <- isolate(input$split.dt)
    if(exists(paste0("prediccion.dt.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.dt.mc))
        output$txtDtMC <- renderPrint(print(exe("MC.dt.",tipo)))
        exe(plot.MC.code())
        output$plot.dt.mc <- renderPlot(isolate(exe("plot.MC(MC.dt.",tipo,")")))
        insert.report(paste0("mc.dt.",tipo),
                      paste0("## Matriz de Confusión del Modelo Árboles de Decisión\n```{r}\n", cod.dt.mc,
                             "\nMC.dt.",tipo,"\n```\n```{r}\nplot.MC(MC.dt.",tipo,")\n```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.dt.",tipo))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.dt(3)
        showNotification(paste0("Error (DT-03) : ", e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.dt.ind <- function() {
    tipo <- isolate(input$split.dt)
    if(exists(paste0("MC.dt.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.dt.ind))
        indices.dt <<- indices.generales(exe("MC.dt.",tipo))

        insert.report(paste0("ind.dt.",tipo), paste0("## Índices Generales \n```{r}\n", cod.dt.ind,
                                                     "\nindices.generales(MC.dt.",tipo,")\n",
                                                     "IndicesM[['dtl-",tipo,"']] <<- indices.generales(MC.dt.",tipo,")\n```"))

        nombres <- c("dtPrecGlob", "dtErrorGlob")
        fill.gauges(nombres, indices.dt)

        # Cambia la tabla con la indices de dt
        output$dtIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.dt,"Árboles de Decisión")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$dtIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.dt,"Árboles de Decisión")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)

        IndicesM[[paste0("dtl-",tipo)]] <<- indices.dt
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.dt(4)
        showNotification(paste0("Error (DT-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE RF ------------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo rf
  observeEvent(input$runRf, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      rf.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$ntree.rf,input$mtry.rf), {
    if (validar.datos(print = FALSE) & rf.stop.excu) {
      deafult.codigo.rf()
    }else{
      rf.stop.excu <<- TRUE
    }
  }, priority =  5)

  observeEvent(input$rules.rf.n,{
    if(validar.datos(print = FALSE)){
        mostrar.reglas.rf(input$rules.rf.n)
    }
  })

  # Acualiza el codigo a la version por defecto
  deafult.codigo.rf <- function(rf.def = FALSE){
    if((!is.null(datos.aprendizaje) & rf.def) | is.na(input$mtry.rf)){
      mtry.value <- ifelse(rf.def || is.na(input$mtry.rf), round(sqrt(ncol(datos.aprendizaje))), input$mtry.rf)
      if(!is.na(input$mtry.rf)){
        updateNumericInput(session,"mtry.rf",value = mtry.value)
      }
    }else{
      mtry.value <- input$mtry.rf
    }

    # Se acualiza el codigo del modelo
    codigo <- rf.modelo(variable.pr = variable.predecir,
                        ntree = input$ntree.rf,
                        mtry = mtry.value)

    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo

    updateAceEditor(session, "fieldCodeRfPlotError", value = plot.rf.error())

    # Se genera el codigo de la prediccion
    codigo <- rf.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo

    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = rf.plot())

    # Se genera el codigo de la matriz
    codigo <- rf.MC(variable.predecir)
    updateAceEditor(session, "fieldCodeRfMC", value = codigo)
    cod.rf.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.rf <- function(capa = NULL){
    for(i in capa:4){
      switch(i, {
        modelo.rf <<- NULL
        output$txtRf <- renderPrint(invisible(""))
        remove.report.elem("modelo.rf")
        remove.report.elem("modelo.rf.graf")
      }, {
        prediccion.rf <<- NULL
        remove.report.elem("pred.rf")
        output$rfPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.rf <<- NULL
        remove.report.elem("mc.rf")
        output$plot.rf.mc <- renderPlot(NULL)
        output$txtRfMC <- renderPrint(invisible(NULL))
      }, {
        indices.rf <<- rep(0, 10)
        remove.report.elem("ind.rf")
        IndicesM[["rfl"]] <<- NULL
      })
    }
  }

  # Grafico de importancia
  plotear.rf.imp <- function() {
    tryCatch({
      output$plot.rf <- renderPlot(isolate(exe(input$fieldCodeRfPlot)))
      cod <- ifelse(input$fieldCodeRfPlot == "", rf.plot(), input$fieldCodeRfPlot)
      insert.report("modelo.rf.graf", paste0("## Importancia de las Variables\n```{r}\n", cod , "\n```"))
    }, error = function(e) {
      output$plot.rf <- renderPlot(NULL)
      insert.report("modelo.rf.graf",NULL)
    })
  }

  plotear.rf.error <- function(){
    tryCatch({
      output$plot.error.rf <- renderPlot(isolate(exe(input$fieldCodeRfPlotError)))
      cod <- ifelse(input$fieldCodeRfPlotError == "", plot.rf.error(),input$fieldCodeRfPlotError)
      insert.report("modelo.rf.error.",
                    paste0("## Evolución del Error del Modelo Bosques Aleatorios\n```{r}\n", cod, "\n```"))
    }, error = function(e){
      limpia.rf(1)
    })
  }

  #Mostrar Reglas
  mostrar.reglas.rf <- function(n){
    output$rulesRf <- renderPrint({
      tryCatch({
          updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,")"))
          printRandomForests(modelo.rf, n)
        },error = function(e){
          stop(tr("NoDRule"))
      })
    })
    insert.report(paste0("modelo.rf.rules.", n), paste0("\n## Reglas del árbol #",n," \n```{r}\nprintRandomForests(modelo.rf, ",n,")\n```"))
  }

  # Ejecuta el modelo, prediccion, mc e indices de rf
  rf.full <- function(){
    ejecutar.rf()
    ejecutar.rf.pred()
    ejecutar.rf.mc()
    ejecutar.rf.ind()
  }

  # Genera el modelo
  ejecutar.rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.modelo))
      output$txtRf <- renderPrint(print(modelo.rf))

      insert.report("modelo.rf",paste0("## Generación del Modelo Bosques Aleatorios\n```{r}\n",
                                       cod.rf.modelo, "\nmodelo.rf\n```"))

      plotear.rf.imp()
      plotear.rf.error()
      mostrar.reglas.rf(input$rules.rf.n)
      nombres.modelos <<- c(nombres.modelos, "modelo.rf")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(1)
      showNotification(paste0("Error (RF-01) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.rf.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.pred))
      scores[["rfl"]] <<- predict(modelo.rf, datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = "prob")
      # Cambia la tabla con la prediccion de rf
      output$rfPrediTable <- DT::renderDataTable(obj.predic(prediccion.rf), server = FALSE)

      insert.report("pred.rf",
                    paste0("## Predicción del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.pred,
                           "\nscores[['rfl']] <<- predict(modelo.rf, datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = 'prob')\n```"))

      nombres.modelos <<- c(nombres.modelos, "prediccion.rf")
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.rf.mc <- function() {
    if(exists("prediccion.rf")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.mc))
        output$txtRfMC <- renderPrint(print(MC.rf))
        isolate(exe(plot.MC.code()))
        output$plot.rf.mc <- renderPlot(isolate(exe("plot.MC(MC.rf)")))

        insert.report("mc.rf",paste0("## Matriz de Confusión del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.mc,
                                     "\nMC.rf\n```\n```{r}\nplot.MC(MC.rf)\n```"))

        nombres.modelos <<- c(nombres.modelos, "MC.rf")
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rf(3)
        showNotification(paste0("Error (RF-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.rf.ind <- function() {
    if(exists("MC.rf")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.ind))
        indices.rf <<- indices.generales(MC.rf)

        insert.report("ind.rf",paste0("## Índices Generales\n```{r}\n",
                                      cod.rf.ind, "\nindices.generales(MC.rf)\n",
                                      "IndicesM[['rfl']] <<- indices.generales(MC.rf)\n```"))

        nombres <- c("rfPrecGlob", "rfErrorGlob")
        fill.gauges(nombres, indices.rf)

        # Cambia la tabla con la indices de rf
        output$rfIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rf,"Bosques Aleatorios")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$rfIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.rf,"Bosques Aleatorios")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, "indices.rf")
        IndicesM[["rfl"]] <<- indices.rf
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rf(4)
        showNotification(paste0("Error (RF-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE BOOSTING ------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo boosting
  observeEvent(input$runBoosting, {
    if (length(levels(datos[, variable.predecir])) == 2) {
      if (validar.datos()){ # Si se tiene los datos entonces :
        boosting.full()
      }
    }else{
      if (isFALSE(getOption("shiny.testmode")) || is.null(getOption("shiny.testmode"))) {
        showModal(modalDialog(
          title = "ADA - BOOSTING", tr("limitModel"),
          footer = modalButton("Cerrar"), easyClose = T
        ))
      }
    }
  }, priority =  -5)

  observeEvent(input$rules.b.n,{
    if(validar.datos(print = FALSE)){
      mostrar.reglas.boosting(input$rules.b.n)
    }
  }, priority =  5)

  # Si las opciones cambian o actualizar el codigo
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting, input$minsplit.boosting, input$maxdepth.boosting), {
    if (validar.datos(print = FALSE) & length(levels(datos[, variable.predecir])) == 2){
      deault.codigo.boosting()
    }
  })

  # Acualiza el codigo a la version por defecto
  deault.codigo.boosting <- function() {
    tipo <- isolate(input$tipo.boosting)

    # Se acualiza el codigo del modelo
    codigo <- boosting.modelo(variable.pr = variable.predecir,
                              iter = input$iter.boosting,
                              maxdepth = input$maxdepth.boosting,
                              type = tipo,
                              minsplit = input$minsplit.boosting)

    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- boosting.prediccion(variable.predecir, tipo)
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo

    # Cambia el codigo del grafico del modelo
    updateAceEditor(session, "fieldCodeBoostingPlot", value = boosting.plot(tipo))

    # Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import(tipo))

    # Se genera el codigo de la matriz
    codigo <- boosting.MC(variable.predecir, tipo)
    updateAceEditor(session, "fieldCodeBoostingMC", value = codigo)
    cod.b.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.boosting <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.boosting.",input$tipo.boosting," <<- NULL")
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting <- renderPlot(NULL)
        output$plot.boosting.import <- renderPlot(NULL)
        remove.report.elem(paste0("modelo.b.",input$tipo.boosting))
        remove.report.elem(paste0("modelo.b.error.",input$tipo.boosting))
        remove.report.elem(paste0("modelo.b.imp.",input$tipo.boosting))
      }, {
        exe("prediccion.boosting.",input$tipo.boosting," <<- NULL")
        remove.report.elem(paste0("pred.b.",input$tipo.boosting))
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.boosting.",input$tipo.boosting," <<- NULL")
        remove.report.elem(paste0("mc.b.",input$tipo.boosting))
        output$plot.boosting.mc <- renderPlot(NULL)
        output$txtBoostingMC <- renderPrint(invisible(NULL))
      }, {
        exe("indices.boosting.",input$tipo.boosting," <<- NULL")
        remove.report.elem(paste0("ind.b.",input$tipo.boosting))
        IndicesM[[paste0("bl-",input$tipo.boosting)]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de knn
  boosting.full <- function() {
    if (length(levels(datos[, variable.predecir])) == 2) {
      ejecutar.boosting()
      ejecutar.boosting.pred()
      ejecutar.boosting.mc()
      ejecutar.boosting.ind()
    }
  }

  # Grafico de boosting
  plotear.boosting <- function() {
    tryCatch({
      tipo <- isolate(input$tipo.boosting)
      output$plot.boosting <- renderPlot(isolate(exe(input$fieldCodeBoostingPlot)))
      cod <- ifelse(input$fieldCodeBoostingPlot == "",boosting.plot(),input$fieldCodeBoostingPlot)
      insert.report(paste0("modelo.b.error.",tipo),
                    paste0("## Evolución del Error - ",tipo,"\n```{r}\n", cod, "\n```"))
    }, error = function(e) {
      limpia.boosting(1)
    })
  }

  # Grafico de importancia
  plotear.boosting.imp <- function() {
    tryCatch({
      tipo <- isolate(input$tipo.boosting)
      output$plot.boosting.import <- renderPlot(isolate(exe(input$fieldCodeBoostingPlotImport)))
      cod <- ifelse(input$fieldCodeBoostingPlotImport == "",boosting.plot.import(),input$fieldCodeBoostingPlotImport)
      insert.report(paste0("modelo.b.imp.",tipo),paste0("## Importancia de las Variables - ",tipo,"\n```{r}\n", cod , "\n```"))
    }, error = function(e) {
      limpia.boosting(1)
    })
  }

  #Mostrar Reglas
  mostrar.reglas.boosting <- function(n){
    output$rulesB <- renderPrint({
      tryCatch({
        tipo <- isolate(input$tipo.boosting)
        updateAceEditor(session,"fieldCodeBoostingRules",rules.boosting(type = tipo, input$rules.b.n))
        exe(rules.boosting(type = tipo, input$rules.b.n))
      },error = function(e) {
          stop(tr("NoDRule"))
      }
    )})
    tipo <- isolate(input$tipo.boosting)
    insert.report(paste0("modelo.b.rules.",input$tipo.boosting,".", n), paste0("\n## Reglas del árbol #",n," \n```{r}\n",
                                                       rules.boosting(type = tipo, input$rules.b.n),"\n```"))
  }

  # Genera el modelo
  ejecutar.boosting <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.modelo))
      tipo <- isolate(input$tipo.boosting)
      output$txtBoosting <- renderPrint(exe("print(modelo.boosting.",tipo,")"))
      plotear.boosting()
      plotear.boosting.imp()
      mostrar.reglas.boosting(input$rules.b.n)
      insert.report(paste0("modelo.b.",tipo),
                    paste0("## Generación del Modelo ADA-BOOSTING - ",tipo,"\n```{r}\n",
                           cod.b.modelo, "\nmodelo.boosting.",tipo,"\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(1)
      showNotification(paste0("Error (B-01) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.boosting.pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.pred))
      tipo <- isolate(input$tipo.boosting)
      scores[[paste0("bl-",tipo)]] <<- predict(exe("modelo.boosting.",tipo), datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = "prob")

      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.boosting.",tipo)),server = FALSE)
      insert.report(paste0("pred.b.",tipo),
                    paste0("## Predicción del Modelo ADA-BOOSTING - ",tipo,"\n```{r}\n",
                    "\nscores[['bl-",tipo,"']] <<- predict(modelo.boosting.",tipo,
                    ", datos.prueba[, -which(colnames(datos.prueba) == variable.predecir)], type = 'prob')\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.boosting.mc <- function() {
    tipo <- isolate(input$tipo.boosting)
    if(exists(paste0("prediccion.boosting.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.mc))
        output$txtBoostingMC <- renderPrint(exe("print(MC.boosting.",tipo,")"))

        exe(plot.MC.code())
        output$plot.boosting.mc <- renderPlot(exe("plot.MC(MC.boosting.",tipo,")"))

        insert.report(paste0("mc.b.",tipo), paste0("## Matriz de Confusión del Modelo ADA-BOOSTING - ",tipo,"\n```{r}\n",
                                                                  cod.b.mc,"\nMC.boosting.",tipo,
                                                                  "\n```\n\n```{r}\nplot.MC(MC.boosting.",tipo,")\n```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.boosting.",tipo))
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.boosting(3)
        showNotification(paste0("Error (B-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.boosting.ind <- function() {
    tipo <- isolate(input$tipo.boosting)
    if(exists(paste0("MC.boosting.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.ind))

        MC <- exe("MC.boosting.",tipo)
        indices.boosting <<- indices.generales(MC)
        exe("indices.boosting.",tipo, "<<- indices.boosting")

        insert.report(paste0("ind.b.",tipo),
                      paste0("## Índices Generales del Modelo ADA-BOOSTING - ",tipo,"\n```{r}\n",
                             cod.b.ind, "\nindices.generales(MC.boosting.",tipo,")\n",
                             "IndicesM[['bl-",tipo,"']] <<- indices.generales(MC.boosting.",tipo,")\n```"))
        nombres <- c("boostingPrecGlob", "boostingErrorGlob")
        fill.gauges(nombres, indices.boosting)

        # Cambia la tabla con la indices de boosting
        output$boostingIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.boosting,"ADA-BOOSTING")), spacing = "xs",
                                                          bordered = T, width = "100%", align = "c", digits = 2)

        output$boostingIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.boosting,"ADA-BOOSTING")), spacing = "xs",
                                                         bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.boosting.",tipo))
        IndicesM[[paste0("bl-",tipo)]] <<- indices.boosting
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.boosting(4)
        showNotification(paste0("Error (B-04) : ", e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE BAYES -------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo bayes
  observeEvent(input$runBayes, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      bayes.full()
    }
  }, priority =  -5)

  # Acualiza el codigo a la version por defecto
  default.codigo.bayes <- function() {
    # Se acualiza el codigo del modelo
    codigo <- bayes.modelo()
    updateAceEditor(session, "fieldCodeBayes", value = codigo)
    cod.bayes.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- bayes.prediccion()
    updateAceEditor(session, "fieldCodeBayesPred", value = codigo)
    cod.bayes.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- bayes.MC()
    updateAceEditor(session, "fieldCodeBayesMC", value = codigo)
    cod.bayes.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeBayesIG", value = codigo)
    cod.bayes.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.bayes <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.bayes <<- NULL
        output$txtbayes <- renderPrint(invisible(""))
        remove.report.elem("modelo.bayes")
      }, {
        prediccion.bayes <<- NULL
        remove.report.elem("pred.bayes")
        output$bayesPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.bayes<<- NULL
        remove.report.elem("mc.bayes")
        output$plot.bayes.mc <- renderPlot(NULL)
        output$txtbayesMC <- renderPrint(invisible(NULL))
      }, {
        indices.bayes <<- NULL
        remove.report.elem("ind.bayes")
        IndicesM[["Bayes"]] <<- indices.bayes
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de bayyes
  bayes.full <- function() {
    ejecutar.bayes()
    ejecutar.bayes.pred()
    ejecutar.bayes.mc()
    ejecutar.bayes.ind()
  }

  # Genera el modelo
  ejecutar.bayes <- function() {
    tryCatch({
      exe(cod.bayes.modelo)
      updateAceEditor(session, "fieldCodeBayes", value = cod.bayes.modelo)
      output$txtbayes <- renderPrint(modelo.bayes)
      insert.report("modelo.bayes",
                    paste0("## Generación del modelo BAYES\n```{r}\n",cod.bayes.modelo, "\nmodelo.bayes\n```"))

      nombres.modelos <<- c(nombres.modelos, "modelo.bayes")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.bayes(1)
      showNotification(paste0("Error (BAYES-01) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.bayes.pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.bayes.pred)
      scores[["Bayes"]] <<- predict(modelo.bayes, datos.prueba, type = "raw")

      # Cambia la tabla con la prediccion de bayes
      output$bayesPrediTable <- DT::renderDataTable(obj.predic(prediccion.bayes), server = FALSE)
      insert.report("pred.bayes",
                    paste0("## Predicción del Modelo BAYES\n```{r}\n", cod.bayes.pred,
                           "\nscores[['Bayes']] <<- predict(modelo.bayes, datos.prueba, type = 'raw')\n```"))

      nombres.modelos <<- c(nombres.modelos, "prediccion.bayes")
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.bayes(2)
      showNotification(paste0("Error (BAYES-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.bayes.mc <- function() {
    if(exists("prediccion.bayes")){
      tryCatch({ # Se corren los codigo
        exe(cod.bayes.mc)
        output$txtbayesMC <- renderPrint(print(MC.bayes))

        exe(plot.MC.code())
        output$plot.bayes.mc <- renderPlot(plot.MC(MC.bayes))
        insert.report("mc.bayes",
                      paste0("## Matriz de confusión del Modelo BAYES\n```{r}\n", cod.bayes.mc,
                             "\nMC.bayes\n```\n```{r}\nplot.MC(MC.bayes)\n```"))

        nombres.modelos <<- c(nombres.modelos, "MC.bayes")
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.bayes(3)
        showNotification(paste0("Error (BAYES-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.bayes.ind <- function(){
    if(exists("MC.bayes")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.bayes.ind))
        indices.bayes <<- indices.generales(MC.bayes)

        insert.report("ind.bayes",
                      paste0("## Índices Generales del Modelo BAYES\n```{r}\n",
                             cod.bayes.ind, "\nindices.generales(MC.bayes)\n",
                             "IndicesM[['Bayes']] <<- indices.generales(MC.bayes)\n```"))

        nombres <- c("bayesPrecGlob", "bayesErrorGlob")
        fill.gauges(nombres, indices.bayes)

        # Cambia la tabla con la indices de bayes
        output$bayesIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.bayes,"BAYES")), spacing = "xs",
                                                       bordered = T, width = "100%", align = "c", digits = 2)
        output$bayesIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.bayes,"BAYES")), spacing = "xs",
                                                      bordered = T, width = "100%", align = "c", digits = 2)
        nombres.modelos <<- c(nombres.modelos, "indices.bayes")
        IndicesM[["Bayes"]] <<- indices.bayes
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.bayes(4)
        showNotification(paste0("Error (BAYES-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE NN ----------------------------------------------------------------------------------------------------------

  observeEvent(c(input$cant.capas.nn, input$segmentButton), {
    if(!is.null(datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      #updateinitSelects("selHoriz", 1:input$cant.capas.nn)
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  })

  # Cuando se genera el modelo nn
  observeEvent(input$runNn, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      nn.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$cant.capas.nn,input$threshold.nn,input$stepmax.nn,input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                 input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10),{
    if(validar.datos(print = FALSE)){
      default.codigo.nn()
      # nn.full()
    }
  }, priority =  5)

  # Acualiza el codigo a la version por defecto
  default.codigo.nn <- function(){
    #Se acualiza el codigo del modelo
    codigo <- nn.modelo(input$threshold.nn,
                        input$stepmax.nn,
                        input$cant.capas.nn,
                        input$nn.cap.1,input$nn.cap.2,
                        input$nn.cap.3,input$nn.cap.4,
                        input$nn.cap.5,input$nn.cap.6,
                        input$nn.cap.7,input$nn.cap.8,
                        input$nn.cap.9,input$nn.cap.10)

    updateAceEditor(session, "fieldCodeNn", value = codigo)
    cod.nn.modelo <<- codigo

    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeNnPlot", value = nn.plot())

    #Se genera el codigo de la prediccion
    codigo <- nn.prediccion()
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)
    cod.nn.pred <<- codigo

    #Se genera el codigo de la matriz
    codigo <- nn.MC()
    updateAceEditor(session, "fieldCodeNnMC", value = codigo)
    cod.nn.mc <<- codigo

    #Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
    cod.nn.ind <<- codigo
  }

  # Plotear el arbol
  plotear.red <- function(){
    tryCatch({
      capas <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
      input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10)
      capas <- capas[1:input$cant.capas.nn]
      if(input$cant.capas.nn * sum(capas) <= 1500 & ncol(modelo.nn$covariate) <= 20){
        output$plot.nn <- renderPlot(isolate(exe(input$fieldCodeNnPlot)))
        cod <- ifelse(input$fieldCodeNnPlot == "", nn.plot(), input$fieldCodeNnPlot)
        insert.report("modelo.nn.graf", paste0("\n```{r}\n", cod, "\n```"))
      }else{
        showNotification(tr("bigPlot"), duration = 10, type = "message")
      }
    },
    error = function(e){
      output$plot.nn <- renderPlot(NULL)
      remove.report.elem("modelo.nn.graf")
    })
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.nn <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.nn <<- NULL
        output$txtnn <- renderPrint(invisible(""))
        output$plot.nn <- renderPlot(NULL)
        remove.report.elem("modelo.nn")
        remove.report.elem("modelo.nn.graf")
      }, {
        prediccion.nn <<- NULL
        remove.report.elem("pred.nn")
        output$nnPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.nn <<- NULL
        remove.report.elem("mc.nn")
        output$plot.nn.mc <- renderPlot(NULL)
        output$txtNnMC <- renderPrint(invisible(NULL))
      }, {
        indices.nn <<- rep(0, 10)
        remove.report.elem("ind.nn")
        IndicesM[["nn"]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de nn
  nn.full <- function() {
    ejecutar.nn()
    if(NN_EXECUTION){
      ejecutar.nn.pred()
      ejecutar.nn.mc()
      ejecutar.nn.ind()
    }
  }

  # Genera el modelo
  ejecutar.nn <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.modelo))
      output$txtnn <- renderPrint(print(modelo.nn))
      insert.report("modelo.nn",
                    paste0("## Generación del modelo Redes Neuronales\n```{r}\n", cod.nn.modelo,
                           "\nsummary(modelo.nn)\n```"))
      plotear.red()
      nombres.modelos <<- c(nombres.modelos,"modelo.nn")
      NN_EXECUTION <<- TRUE
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.nn(1)
      showNotification(paste0("Error (NN-01) : ",e), duration = 15, type = "error")
    },
    warning = function(w){
      limpia.nn(1)
      NN_EXECUTION <<- FALSE
      showNotification(paste0(tr("nnWar")," (NN-01) : ",w), duration = 10, type = "warning")
    })
  }

  # Genera la prediccion
  ejecutar.nn.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.pred))

      # Cambia la tabla con la prediccion de nn
      output$nnPrediTable <- DT::renderDataTable(obj.predic(max.col(prediccion.nn)),server = FALSE)

      insert.report("pred.nn",
                    paste0("## Predicción del Modelo Redes Neuronales\n```{r}\n", cod.nn.pred,
                           "\nscores[['nn']] <<- prediccion.nn\n",
                           "prediccion.nn <<- max.col(prediccion.nn)\n```"))

      nombres.modelos <<- c(nombres.modelos,"prediccion.nn")
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc

      scores[["nn"]] <<- prediccion.nn
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.nn(2)
      showNotification(paste0("Error (NN-02) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.nn.mc <- function() {
    if(exists("prediccion.nn")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.nn.mc))
        output$txtnnMC <- renderPrint(print(MC.nn))
        exe(plot.MC.code())
        output$plot.nn.mc <- renderPlot(isolate(exe(plot.MC(MC.nn))))
        insert.report("mc.nn",
                      paste0("## Matriz de Confusión del Modelo Redes Neuronales\n```{r}\n", cod.nn.mc,
                             "\nMC.nn\n```\n```{r}\nplot.MC(MC.nn)\n```"))

        nombres.modelos <<- c(nombres.modelos, "MC.nn")
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        limpia.nn(3)
        showNotification(paste0("Error (NN-03) : ", e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.nn.ind <- function() {
    if(exists("MC.nn")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.nn.ind))
        indices.nn <<- indices.generales(MC.nn)

        insert.report("ind.nn", paste0("## Índices Generales \n```{r}\n", cod.nn.ind,
                                       "\nindices.generales(MC.nn)\n",
                                       "IndicesM[['nn']] <<- indices.generales(MC.nn)\n```"))

        nombres <- c("nnPrecGlob", "nnErrorGlob")
        fill.gauges(nombres, indices.nn)

        # Cambia la tabla con la indices de nn
        output$nnIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.nn,"Redes Neuronales")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$nnIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.nn,"Redes Neuronales")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)

        IndicesM[["nn"]] <<- indices.nn
        actualizar.selector.comparativa()
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        limpia.nn(4)
        showNotification(paste0("Error (NN-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE GX BOSTING --------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo xgb
  observeEvent(input$runXgb, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      xgb.full()
    }
  }, priority =  -5)

  # Si las opciones cambian
  observeEvent(c(input$boosterXgb, input$maxdepthXgb, input$nroundsXgb), {
    if (validar.datos(print = FALSE)) {
      default.codigo.xgb()
    }
  }, priority =  5)

  # Acualiza el codigo a la version por defecto
  default.codigo.xgb <- function() {
    tipo <- isolate(input$boosterXgb)
    # Se acualiza el codigo del modelo
    codigo <- xgb.modelo(booster = tipo,
                         max.depth = input$maxdepthXgb,
                         n.rounds = input$nroundsXgb)
    updateAceEditor(session, "fieldCodeXgb", value = codigo)
    cod.xgb.modelo <<- codigo

    #Codigo de importancia de variables
    updateAceEditor(session, "fieldCodeXgbImp", value = xgb.varImp(booster = tipo))

    # Se genera el codigo de la prediccion
    codigo <- xgb.prediccion(booster = tipo)
    updateAceEditor(session, "fieldCodeXgbPred", value = codigo)
    cod.xgb.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- xgb.MC(booster = tipo)
    updateAceEditor(session, "fieldCodeXgbMC", value = codigo)
    cod.xgb.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeXgbIG", value = codigo)
    cod.xgb.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.xgb <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        exe("modelo.xgb.",input$boosterXgb," <<- NULL")
        output$txtxgb <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.xgb.",input$boosterXgb))
        remove.report.elem(paste0("modelo.xgb.graf.",input$boosterXgb))
      }, {
        exe("prediccion.xgb.",input$boosterXgb," <<- NULL")
        remove.report.elem(paste0("pred.xgb.",input$boosterXgb))
        output$xgbPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.xgb.",input$boosterXgb," <<- NULL")
        remove.report.elem(paste0("mc.xgb.",input$boosterXgb))
        output$plot.xgb.mc <- renderPlot(NULL)
        output$txtxgbMC <- renderPrint(invisible(NULL))
      }, {
        exe("indices.xgb.",input$boosterXgb," <<- NULL")
        remove.report.elem(paste0("ind.xgb.",input$boosterXgb))
        IndicesM[[paste0("xgb-",input$boosterXgb)]] <<- NULL
      })
    }
  }

  # Grafico de importancia
  plotear.xgb.imp <- function() {
    tryCatch({
      isolate(tipo <- input$boosterXgb)
      isolate(codigo <- input$fieldCodeXgbImp)
      output$plot.xgb <- renderPlot(isolate(exe(codigo)))
      cod <- ifelse(codigo == "", xgb.varImp(booster = tipo), codigo)
      insert.report(paste0("modelo.xgb.graf.",tipo), paste0("## Importancia de las Variables\n```{r}\n", cod , "\n```"))
    }, error = function(e) {
      output$plot.xgb <- renderPlot(NULL)
      remove.report.elem(paste0("modelo.xgb.graf.",tipo))
    })
  }

  # Ejecuta el modelo, prediccion, mc e indices de xgb
  xgb.full <- function() {
    ejecutar.xgb()
    ejecutar.xgb.pred()
    ejecutar.xgb.mc()
    ejecutar.xgb.ind()
  }

  # Genera el modelo
  ejecutar.xgb <- function(){
    tryCatch({
      exe(cod.xgb.modelo)
      tipo <- isolate(input$boosterXgb)
      updateAceEditor(session, "fieldCodeXgb", value = cod.xgb.modelo)
      output$txtxgb <- renderPrint(exe("modelo.xgb.",tipo))
      insert.report(paste0("modelo.xgb.",tipo),
                    paste0("## Generación del modelo XGB - ",tipo,"\n```{r}\n",cod.xgb.modelo, "\nmodelo.xgb.",tipo,"\n```"))

      plotear.xgb.imp()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.xgb.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.xgb(1)
      showNotification(paste0("Error (XGB-01) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.xgb.pred <- function(){
    tryCatch({ # Se corren los codigo
      exe(cod.xgb.pred)
      tipo <- isolate(input$boosterXgb)
      scores[[paste0("xgb-",tipo)]] <<- exe("prediccion.xgb.",tipo)

      num.class <- length(levels(datos.aprendizaje[,variable.predecir]))
      if(num.class > 2){
        pred <- paste0("max.col(matrix(prediccion.xgb.",tipo,", ncol=",num.class,", byrow=TRUE))")
      }else{
        pred <- paste0("ifelse(prediccion.xgb.",tipo," > 0.5, 2, 1)")
      }
      # Cambia la tabla con la prediccion de xgb
      output$xgbPrediTable <- DT::renderDataTable(obj.predic(exe(pred)),server = FALSE)
      insert.report(paste0("pred.xgb.",tipo),
                    paste0("## Predicción del Modelo XGB - ",tipo,"\n```{r}\n", cod.xgb.pred,
                           "\nscores[['",paste0("xgb-",tipo),"']] <<- prediccion.xgb.",tipo,"\n```"))

      nombres.modelos <<- c(nombres.modelos,
                            paste0("prediccion.xgb.",tipo),
                            paste0("valor.var.xgb.",tipo))

      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.xgb(2)
      showNotification(paste0("Error (XGB-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.xgb.mc <- function() {
    tipo <- isolate(input$boosterXgb)
    if(exists(paste0("prediccion.xgb.",tipo))){
      tryCatch({ # Se corren los codigo
        exe(cod.xgb.mc)
        output$txtxgbMC <- renderPrint(print(exe("MC.xgb.",tipo)))

        exe(plot.MC.code())
        output$plot.xgb.mc <- renderPlot(exe("plot.MC(MC.xgb.",tipo,")"))
        insert.report(paste0("mc.xgb.",tipo),
                      paste0("## Matriz de confusión del Modelo XGB - ",
                             tipo,"\n```{r}\n", cod.xgb.mc,
                             "\nMC.xgb.",tipo,
                             "\n```\n```{r}\nplot.MC(MC.xgb.",tipo,")```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.xgb.",tipo))
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        limpia.xgb(3)
        showNotification(paste0("Error (XGB-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.xgb.ind <- function(){
    tipo <- isolate(input$boosterXgb)
    if(exists(paste0("MC.xgb.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.xgb.ind))
        indices.xgb <- indices.generales(exe("MC.xgb.",tipo))
        eval(parse(text = paste0("indices.xgb.",tipo, "<<- indices.xgb")))

        insert.report(paste0("ind.xgb.",tipo),
                      paste0("## Índices Generales del Modelo XGB - ",tipo,"\n```{r}\n",
                             cod.xgb.ind, "\nindices.generales(MC.xgb.",tipo,")\n",
                             "IndicesM[['xgb-",tipo,"']] <<- indices.generales(MC.xgb.",tipo,")\n```"))

        nombres <- c("xgbPrecGlob", "xgbErrorGlob")
        fill.gauges(nombres, indices.xgb)

        # Cambia la tabla con la indices de xgb
        output$xgbIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.xgb,"XGB")), spacing = "xs",
                                                     bordered = T, width = "100%", align = "c", digits = 2)

        output$xgbIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.xgb,"XGB")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.xgb.",tipo))
        IndicesM[[paste0("xgb-",tipo)]] <<- indices.xgb
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.xgb(4)
        showNotification(paste0("Error (XGB-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE RL -------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo rl
  observeEvent(input$runRl, {
    if (length(levels(datos[, variable.predecir])) == 2) {
      if (validar.datos()){ # Si se tiene los datos entonces :
        rl.full()
      }
    }else{
      if (isFALSE(getOption("shiny.testmode")) || is.null(getOption("shiny.testmode"))) {
        showModal(modalDialog(
          title = "Regresión Logística", tr("limitModel"),
          footer = modalButton("Cerrar"), easyClose = T
        ))
      }
    }
  }, priority =  -5)

  # Acualiza el codigo a la version por defecto
  default.codigo.rl <- function() {
    # Se acualiza el codigo del modelo
    codigo <- rl.modelo()
    updateAceEditor(session, "fieldCodeRl", value = codigo)
    cod.rl.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- rl.prediccion()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)
    cod.rl.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- rl.MC()
    updateAceEditor(session, "fieldCodeRlMC", value = codigo)
    cod.rl.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
    cod.rl.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.rl <- function(capa = NULL) {
    for (i in capa:4) {
      switch(i, {
        modelo.rl <<- NULL
        output$txtrl <- renderPrint(invisible(""))
        remove.report.elem("modelo.rl")
      }, {
        prediccion.rl <<- NULL
        remove.report.elem("pred.rl")
        output$rlPrediTable <- DT::renderDataTable(NULL)
      }, {
        MC.rl<<- NULL
        remove.report.elem("mc.rl")
        output$plot.rl.mc <- renderPlot(NULL)
        output$txtrlMC <- renderPrint(invisible(NULL))
      }, {
        indices.rl <<- NULL
        remove.report.elem("ind.rl")
        IndicesM[["rl"]] <<- NULL
      })
    }
  }

  # Ejecuta el modelo, prediccion, mc e indices de bayyes
  rl.full <- function() {
    if (length(levels(datos[, variable.predecir])) == 2) {
      ejecutar.rl()
      ejecutar.rl.pred()
      ejecutar.rl.mc()
      ejecutar.rl.ind()
    }
  }

  # Genera el modelo
  ejecutar.rl <- function() {
    tryCatch({
      exe(cod.rl.modelo)
      updateAceEditor(session, "fieldCodeRl", value = cod.rl.modelo)
      output$txtrl <- renderPrint(modelo.rl)
      insert.report("modelo.rl",
                    paste0("## Generación del modelo RL\n```{r}\n",cod.rl.modelo, "\nmodelo.rl\n```"))

      nombres.modelos <<- c(nombres.modelos, "modelo.rl")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rl(1)
      showNotification(paste0("Error (RL-01) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.rl.pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.rl.pred)
      scores[["rl"]] <<- predict(modelo.rl, datos.prueba, type = 'response')

      # Cambia la tabla con la prediccion de rl
      output$rlPrediTable <- DT::renderDataTable(obj.predic(prediccion.rl), server = FALSE)
      insert.report("pred.rl",
                    paste0("## Predicción del Modelo RL\n```{r}\n", cod.rl.pred,
                           "\nscores[['rl']] <<- predict(modelo.rl, datos.prueba, type = 'response')\n```"))

      nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.rl.mc <- function() {
    if(exists("prediccion.rl")){
      tryCatch({ # Se corren los codigo
        exe(cod.rl.mc)
        output$txtrlMC <- renderPrint(print(MC.rl))

        exe(plot.MC.code())
        output$plot.rl.mc <- renderPlot(plot.MC(MC.rl))
        insert.report("mc.rl",
                      paste0("## Matriz de confusión del Modelo RL\n```{r}\n", cod.rl.mc,
                             "\nMC.rl\n```\n```{r}\nplot.MC(MC.rl)\n```"))

        nombres.modelos <<- c(nombres.modelos, "MC.rl")
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rl(3)
        showNotification(paste0("Error (RL-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.rl.ind <- function(){
    if(exists("MC.rl")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rl.ind))
        indices.rl <<- indices.generales(MC.rl)

        insert.report("ind.rl",
                      paste0("## Índices Generales del Modelo RL\n```{r}\n",
                             cod.rl.ind, "\nindices.generales(MC.rl)\n",
                             "IndicesM[['rl']] <<- indices.generales(MC.rl)\n```"))

        nombres <- c("rlPrecGlob", "rlErrorGlob")
        fill.gauges(nombres, indices.rl)

        # Cambia la tabla con la indices de rl
        output$rlIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rl,"RL")), spacing = "xs",
                                                       bordered = T, width = "100%", align = "c", digits = 2)
        output$rlIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.rl,"RL")), spacing = "xs",
                                                      bordered = T, width = "100%", align = "c", digits = 2)
        nombres.modelos <<- c(nombres.modelos, "indices.rl")
        IndicesM[["rl"]] <<- indices.rl
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rl(4)
        showNotification(paste0("Error (RL-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # PAGINA DE RLR -----------------------------------------------------------------------------------------------------------

  # Cuando se genera el modelo rlr
  observeEvent(input$runRlr, {
    if (validar.datos()) { # Si se tiene los datos entonces :
      rlr.full()
    }
  })

  # Si las opciones cambian
  observeEvent(c(input$alpha.rlr, input$switch.scale.rlr, input$landa, input$permitir.landa), {
    if (validar.datos(print = FALSE)) {
      default.codigo.rlr()
    }
  })

  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })

  get_landa_rlr <- function(){
    landa <- NULL
    if (!is.na(input$landa) && input$permitir.landa) {
      if (input$landa > 0) {
        landa <- input$landa
      }
    }
    return(landa)
  }

  # Acualiza el codigo a la version por defecto
  default.codigo.rlr <- function(){
    landa <- get_landa_rlr()

    # Se acualiza el codigo del modelo
    codigo <- rlr.modelo(variable.pr = variable.predecir,
                         input$alpha.rlr,
                         input$switch.scale.rlr)

    updateAceEditor(session, "fieldCodeRlr", value = codigo)
    cod.rlr.modelo <<- codigo

    # Se genera el codigo del posible landa
    codigo <- select.landa(variable.predecir,
                           input$alpha.rlr,
                           input$switch.scale.rlr)

    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
    cod.select.landa <<- codigo

    #Se genera el codigo de los coeficientes con el mejor landa
    codigo <- plot.coeff.landa(landa)
    updateAceEditor(session, "fieldCodeRlrLanda", value = codigo)

    # Se genera el codigo de la prediccion
    codigo <- rlr.prediccion(variable.predecir,landa)
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)
    cod.rlr.pred <<- codigo

    # Se genera el codigo de la matriz
    codigo <- rlr.MC()
    updateAceEditor(session, "fieldCodeRlrMC", value = codigo)
    cod.rlr.mc <<- codigo

    # Se genera el codigo de la indices
    codigo <- extract.code("indices.generales")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
    cod.rlr.ind <<- codigo
  }

  # Limpia los datos segun el proceso donde se genera el error
  limpia.rlr <- function(capa = NULL){
    tipo <- rlr.type()
    for(i in capa:4){
      switch(i, {
        modelo.rlr <<- NULL
        output$txtRlr <- renderPrint(invisible(""))
        remove.report.elem(paste0("posib.landa.rlr.",tipo))
        remove.report.elem(paste0("gcoeff.landa.rlr.",tipo))
      }, {
        prediccion.rlr <<- NULL
        remove.report.elem(paste0("pred.rlr.",tipo))
        output$rlrPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("MC.rlr.",tipo," <<- NULL")
        remove.report.elem(paste0("mc.rlr.",tipo))
        output$plot.rlr.mc <- renderPlot(NULL)
        output$txtrlrMC <- renderPrint(invisible(NULL))
      },{
        indices.rlr <<- rep(0, 10)
        remove.report.elem(paste0("ind.rlr.",tipo))
      })
    }
  }

  plot.posib.landa.rlr <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.select.landa))
      isolate(tipo <- rlr.type())
      output$plot.rlr.posiblanda <- renderPlot(exe("plot(cv.glm.",tipo,")"))
      insert.report(paste0("posib.landa.rlr.",tipo),
                    paste0("\n##Posibles lambda\n```{r}\n", cod.select.landa,"\n",
                           "plot(cv.glm.",tipo,")\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }

  plot.coeff <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rlr.type())
      isolate(codigo <- input$fieldCodeRlrLanda)
      output$plot.rlr.landa <- renderPlot(isolate(exe(codigo)))
      insert.report(paste0("gcoeff.landa.rlr.",tipo),
                    paste0("\n##Coeficientes y lambda\n```{r}\nplot.new()\n", codigo,"\n```\n"))
    },
    error = function(e){ # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }

  # Ejecuta el modelo, prediccion, mc e indices de rlr
  rlr.full <- function(){
    ejecutar.rlr()
    ejecutar.rlr.pred()
    ejecutar.rlr.mc()
    ejecutar.rlr.ind()
  }

  # Genera el modelo
  ejecutar.rlr <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.modelo))
      isolate(tipo <- rlr.type())
      output$txtRlr <- renderPrint(print(exe("modelo.rlr.",tipo)))

      insert.report(paste0("modelo.rlr.",tipo),paste0("## Generación del Modelo R/L\n```{r}\n",
                                                            cod.rlr.modelo, "\nmodelo.rlr.",tipo,"\n```"))

      plot.posib.landa.rlr()
      plot.coeff()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rlr.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(1)
      showNotification(paste0("Error (R/L-01) : ",e), duration = 15, type = "error")
    })
  }

  # Genera la prediccion
  ejecutar.rlr.pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.pred))
      landa <- get_landa_rlr()
      isolate(tipo <- rlr.type())
      landa <- ifelse(is.null(landa),paste0("cv.glm.",tipo,"$lambda.min"), landa)
      scores[[paste0("rlr-",tipo)]] <<- exe("prueba <- model.matrix(",variable.predecir,"~., datos.prueba)[, -1]\n",
                                                  "predict(modelo.rlr.",tipo,", prueba, s = ",landa,", type='response')")

      # Cambia la tabla con la prediccion de rlr
      output$rlrPrediTable <- DT::renderDataTable(obj.predic(exe("prediccion.rlr.",tipo)), server = FALSE)

      insert.report(paste0("pred.rlr.",tipo),
                    paste0("## Predicción del R/L\n```{r}\n", cod.rlr.pred,
                           "\nprueba <- model.matrix(",variable.predecir,"~., datos.prueba)[, -1]",
                           "\nscores[['rlr-",tipo,"']] <<- predict(modelo.rlr.",tipo,", prueba, type = 'response', s = ",landa,")\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.rlr.",tipo))
      updatePlot$roc <- !updatePlot$roc #graficar otra vez la curva roc
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      limpia.rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }

  # Genera la matriz de confusion
  ejecutar.rlr.mc <- function() {
    isolate(tipo <- rlr.type())
    if(exists(paste0("prediccion.rlr.",tipo))){
      tryCatch({ # Se corren los codigo
        exe(cod.rlr.mc)
        output$txtrlrMC <- renderPrint(print(exe("MC.rlr.",tipo)))

        exe(plot.MC.code())
        output$plot.rlr.mc <- renderPlot(exe("plot.MC(MC.rlr.",tipo,")"))
        insert.report(paste0("mc.rlr.",tipo),
                      paste0("## Matriz de confusión del Modelo XGB - ",
                             tipo,"\n```{r}\n", cod.rlr.mc,
                             "\nMC.rlr.",tipo,
                             "\n```\n```{r}\nplot.MC(MC.rlr.",tipo,")```"))

        nombres.modelos <<- c(nombres.modelos, paste0("MC.rlr.",tipo))
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        limpia.rlr(3)
        showNotification(paste0("Error (RLR-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # Genera los indices
  ejecutar.rlr.ind <- function() {
    isolate(tipo <- rlr.type())
    if(exists(paste0("prediccion.rlr.",tipo))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rlr.ind))
        indices.rlr <- exe("indices.generales(MC.rlr.",tipo,")")

        insert.report(paste0("ind.rlr.",tipo),
                      paste0("## Índices Generales\n```{r}\n",
                             cod.rlr.ind, "\nindices.generales(MC.rlr.",tipo,")\n",
                             "IndicesM[['rlr-",tipo,"']] <<- indices.generales(MC.rlr.",tipo,")\n```"))

        nombres <- c("rlrPrecGlob", "rlrErrorGlob")
        fill.gauges(nombres, indices.rlr)

        # Cambia la tabla con la indices de rl
        output$rlrIndPrecTable <- shiny::renderTable(xtable(indices.prec.table(indices.rlr,"RLR")), spacing = "xs",
                                                    bordered = T, width = "100%", align = "c", digits = 2)
        output$rlrIndErrTable <- shiny::renderTable(xtable(indices.error.table(indices.rlr,"RLR")), spacing = "xs",
                                                   bordered = T, width = "100%", align = "c", digits = 2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.rlr.",tipo))
        IndicesM[[paste0("rlr-",tipo)]] <<- indices.rlr
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        limpia.rlr(4)
        showNotification(paste0("Error (R/L-04) : ",e), duration = 15, type = "error")
      })
    }
  }

  # TABLA COMPARATIVA -------------------------------------------------------------------------------------------------------

  split_name <-function(name){
    nom.aux <- unlist(strsplit(name, "-"))
    ifelse(length(nom.aux) == 1,
           tr(nom.aux),
           paste0(tr(nom.aux[1]),"-",nom.aux[2]))
  }

  get_names_models <- function(){
    if(length(IndicesM) == 0) {
      return("---X---")
    }
    nombres <- c()
    for (nom in names(IndicesM)){
      nombres <- c(nombres,split_name(nom))
    }
    return(nombres)
  }

  #Actualiza los selectores de la tabla comparativa
  actualizar.selector.comparativa <- function(){
    nombres <- get_names_models()
    shinyWidgets::updateCheckboxGroupButtons(session,"select.models",choices = sort(nombres),selected = sort(nombres),
                                             status = "primary",checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                                 no = icon("remove", lib = "glyphicon")))
  }

  # Crea la tabla comparativa
  tabla.comparativa <- function(sel) {
    tryCatch({
      nombres <- get_names_models()

      if(nombres[1] == "---X---") {
        return(data.frame())
      }
      resp <- lapply(IndicesM, function(x)t(as.data.frame(unlist(x))))
      resp <- as.data.frame(do.call(rbind, resp))
      selector <- (ncol(resp)-num.categorias.pred() + 1):ncol(resp)
      resp <- resp[,-selector,drop = FALSE]
      resp <- cbind(resp, replace(unlist(areas[nombres]),is.null(unlist(areas[nombres])),NA))
      rownames(resp) <- nombres
      colnames(resp) <- c(tr('precG'),tr("errG"), levels(datos.aprendizaje[,variable.predecir]), tr('aROC'))
      resp[] <- lapply(resp, as.numeric)
      resp <- round(resp, 4)
      resp <- resp[nombres %in% sel,]
      return(resp)

    }, error = function(e){
      return(data.frame())
    })
  }

  # Calcula las areas de la curva roc de todos los modelos
  calcular.areas <- function(sel) {
    clase <- datos.prueba[, variable.predecir]
    if(length(unique(clase)) == 2){
      for(nombre in names(scores)){
        nom.aux <- split_name(nombre)
        if (is.factor(scores[[nombre]])) {
          areas[[nom.aux]] <<- areaROC(attributes(scores[[nombre]])$probabilities[, sel], clase)
        }else{
          if(is.vector((scores[[nombre]]))){
            areas[[nom.aux]] <<- areaROC(scores[[nombre]], clase)
          }else{
            areas[[nom.aux]] <<- areaROC(as.data.frame(scores[[nombre]])[, which(levels(clase) == sel)], clase)
          }
        }
      }
    }
  }

  #Hace el grafico de la curva roc
  output$plot.roc <- renderPlot({
    graficar <- updatePlot$roc
    if(!is.null(datos.prueba) & length(levels(datos[,variable.predecir])) == 2) {
      calcular.areas(input$roc.sel)
      insert.report("roc",paste0("## Curva ROC \n```{r}\ncalcular.areas('",input$roc.sel,"')\nplotROC( ",as.string.c(input$select.models)," )\n```"))
      plotROC(input$select.models)
    } else {
      insert.report("roc", NULL)
      showNotification(tr("RocNo"), duration = 15, type = "warning")
      return(NULL)
    }
  })

  #Muestra la tabla comparativa.
  output$TablaComp <- DT::renderDataTable({

    graficar <- updatePlot$roc
    if (!is.null(datos.aprendizaje)) {
      calcular.areas(input$roc.sel)
      insert.report("tabla.comparativa",paste0("## Tabla Comparativa \n```{r}\ncalcular.areas('",input$roc.sel,"')\ntabla.comparativa( ",
                                               as.string.c(input$select.models)," )\n```"))
      DT::datatable(tabla.comparativa(input$select.models),
                    selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    }
  },server = FALSE)

  #Si cambian los parametros
  observeEvent(c(input$select.models, input$roc.sel), {
    updatePlot$roc <- !updatePlot$roc
  })

  # PAGINA DE PREDICCIONES NUEVAS -------------------------------------------------------------------------------------------

  varificar.datos.pn <- function(){
    if(any(!(c(colnames(datos.prueba.completos),variable.predecir.pn) %in% colnames(datos.originales.completos))))
      stop(tr("NoTamColum"))
  }

  unificar.factores <- function(){
    for(nombre in colnames(datos.prueba.completos)){
      if(class(datos.prueba.completos[,nombre]) == "factor"){
        levels(datos.prueba.completos[,nombre]) <<- unique(c(levels(datos.prueba.completos[,nombre]),
                                                            levels(datos.aprendizaje.completos[,nombre])))
      }
    }
  }

  actualizar.tabla.pn <- function(tablas = c("contentsPred", "contentsPred2")){
    if("contentsPred2" %in% tablas){
      output$contentsPred <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje.completos,
                                                                 editable = F,
                                                                 scrollY = "25vh"), server = F)
    }
    if("contentsPred2" %in% tablas){
      output$contentsPred2 <- DT::renderDT(renderizar.tabla.datos(datos.aprendizaje.completos,
                                                                  editable = F,
                                                                  scrollY = "25vh"), server = F)
    }
    if("contentsPred3" %in% tablas){
      output$contentsPred3 <- DT::renderDT(renderizar.tabla.datos(datos.prueba.completos,
                                                                  editable = F,
                                                                  scrollY = "25vh"), server = T)
    }
  }

  actualizar.texto.modelo.pn <- function(codigo){
    updateAceEditor(session, "fieldPredNuevos", value = codigo)
    if(is.null(modelo.nuevos)){
      output$txtPredNuevos <- renderPrint(invisible(NULL))
    }else{
      output$txtPredNuevos <- renderPrint(print(modelo.nuevos))
    }
  }

  crear.datos.np <- function(){
    datos.aux.prueba <- datos.prueba.completos
    datos.aux.prueba[,variable.predecir.pn] <- predic.nuevos
    return(datos.aux.prueba)
  }

  actualizar.pred.pn <- function(codigo){
    updateAceEditor(session, "fieldCodePredPN", value = codigo)
    if(!is.null(predic.nuevos)){
      datos.aux.prueba <- crear.datos.np()
      output$PrediTablePN <- DT::renderDT(renderizar.tabla.datos(datos.aux.prueba,
                                                                 editable = F,
                                                                 scrollY = "25vh"),server = T)
    }else{
      output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
    }
  }

  actualizar.nn.capas.np <- function(){
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  }

  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$file3$name
    },
    content = function(file) {
      if(!is.null(predic.nuevos)){
        write.csv(crear.datos.np(), file, row.names = input$rownameNPred2)
      }
    }
  )

  observeEvent(c(input$cant.capas.nn.pred), {
    actualizar.nn.capas.np()
  })

  observeEvent(input$loadButtonNPred,{
    codigo.carga <- code.carga(nombre.filas = input$rownameNPred,
                               ruta = input$file2$datapath,
                               separador = input$sepNPred,
                               sep.decimal = input$decNPred,
                               encabezado = input$headerNPred,
                               d.o = "datos.originales.completos",
                               d = "datos.aprendizaje.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos.originales.completos) <= 1) {
        showNotification(tr("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      codigo.na <- ""
      codigo.na <- paste0(code.NA(deleteNA = input$deleteNAnPred, d.o = "datos.originales.completos"), "\n", "datos.aprendizaje.completos <<- datos.originales.completos")
      isolate(exe( codigo.na))

      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.categoricas(datos.aprendizaje.completos))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(datos.aprendizaje.completos))))
      updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))
    },
    error = function(e) {
      showNotification(paste0("Error:", e), duration = 10, type = "error")
      datos.aprendizaje.completos <<- NULL
      datos.originales.completos <<- NULL
      return(NULL)
    })

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    actualizar.pred.pn("")

    actualizar.texto.modelo.pn("")
    actualizar.tabla.pn()
  })

  update.trans.pn <- eventReactive(c(input$loadButtonNPred), {
    contadorPN <<- contadorPN + 1
    if (!is.null(datos.aprendizaje.completos) && ncol(datos.aprendizaje.completos) > 0) {
      res <- data.frame(Variables = colnames(datos.aprendizaje.completos),
                        Tipo = c(1:ncol(datos.aprendizaje.completos)),
                        Activa = c(1:ncol(datos.aprendizaje.completos)))
      res$Tipo <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0(
        '<select id="Predsel', i, contadorPN, '"> <option value="categorico">',tr("categorico"),'</option>',
        '<option value="numerico" ', ifelse(class(datos.aprendizaje.completos[, i]) %in% c("numeric", "integer"),' selected="selected"', ""),'>', tr("numerico"),'</option>',
        '<option value="disyuntivo">',tr("disyuntivo"),'</option> </select>'
      ))
      res$Activa <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0('<input type="checkbox" id="Predbox', i, contadorPN, '" checked/>'))
      actualizar.nn.capas.np()
    } else {
      res <- as.data.frame(NULL)
      showNotification(tr("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })

  output$transDataPredN <- DT::renderDataTable(update.trans.pn(),
                                          escape = FALSE, selection = "none", server = FALSE,
                                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "35vh"), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))

  transformar.datos.pn <- function() {
    var.noactivas <- c()
    code.res <- "datos.aprendizaje.completos <<- datos.originales.completos \n"
    for (var in colnames(datos.originales.completos)) {
      if (input[[paste0("Predbox", var, contadorPN)]]) {
        if (input[[paste0("Predsel", var, contadorPN)]] == "categorico" & class(datos.originales.completos[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "numerico" & !(class(datos.originales.completos[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico",  d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }


    isolate(exe(code.res))
    if (length(var.noactivas) > 0) {
      des <- code.desactivar(var.noactivas,"datos.aprendizaje.completos")
      isolate(exe(des))
      code.res <- paste0(code.res, "\n", des)
    }
    code.res <- paste0(code.res, "\n")
    return(code.res)
  }

  predecir.pn <-function(){
    if(!is.null(datos.prueba.completos)){
      if(exists("modelo.nuevos") && !is.null(modelo.nuevos)){
        codigo <- switch(modelo.seleccionado.pn,
                         knn =  kkn.prediccion.pn(),
                         dt  = dt.prediccion.np(),
                         rf  = rf.prediccion.np(),
                         ada = boosting.prediccion.np(),
                         svm = svm.prediccion.np(),
                         bayes = bayes.prediccion.np(),
                         xgb = xgb.prediccion.np(),
                         nn = nn.prediccion.np(),
                         rl = rl.prediccion.np(),
                         rlr = rlr.prediccion.np(input$alpha.rlr.pred,input$switch.scale.rlr.pred,
                                                 input$permitir.landa.pred, input$landa.pred))

        tryCatch({
          exe(codigo)
          actualizar.pred.pn(codigo)
        },
        error =  function(e){
          showNotification(paste0("Error :", e), duration = 10, type = "error")
        })
      }else{
        showNotification(paste0("Error :", tr("ErrorModelo")), duration = 10, type = "error")
      }
    }else{
       showNotification(paste0("Error :", tr("ErrorDatosPN")), duration = 10, type = "error")
    }
  }


  observeEvent(input$predecirPromidat, {
    predecir.pn()
  })

  observeEvent(input$transButtonPredN, {
    # transforma los datos
    code.trans.pn <<- transformar.datos.pn()

    # Actualiza los selectores que dependen de los datos
    updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.categoricas(datos.aprendizaje.completos))))
    updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    actualizar.pred.pn("")

    actualizar.texto.modelo.pn("")
    actualizar.tabla.pn()
  })

  observeEvent(input$PredNuevosBttnModelo,{
    variable.predecir.pn <<- input$sel.predic.var.nuevos
    codigo <- switch(input$selectModelsPred,
                     knn =  kkn.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                          scale = input$switch.scale.knn.pred,
                                          kmax = input$kmax.knn.pred,
                                          kernel = input$kernel.knn.pred),
                     dt  = dt.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                        minsplit = input$minsplit.dt.pred,
                                        maxdepth = input$maxdepth.dt.pred,
                                        split = input$split.dt.pred),
                     rf  = rf.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                        ntree = input$ntree.rf.pred,
                                        mtry = input$mtry.rf.pred),
                     ada = boosting.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                              iter = input$iter.boosting.pred,
                                              maxdepth = input$maxdepth.boosting.pred,
                                              type = input$tipo.boosting.pred,
                                              minsplit = input$minsplit.boosting.pred),
                     svm = svm.modelo.np(variable.pr =input$sel.predic.var.nuevos,
                                         scale = input$switch.scale.svm.pred,
                                         kernel = input$kernel.svm.pred),
                     bayes = bayes.modelo.np(variable.pr=input$sel.predic.var.nuevos),
                     xgb = xgb.modelo.np(variable.pr=input$sel.predic.var.nuevos,
                                         booster = input$boosterXgb.pred,
                                         max.depth = input$maxdepthXgb.pred,
                                         n.rounds = input$nroundsXgb.pred),
                     nn = nn.modelo.np(variable.pr=input$sel.predic.var.nuevos,
                                        input$threshold.nn.pred,
                                        input$stepmax.nn.pred,
                                        input$cant.capas.nn.pred,
                                        input$nn.cap.pred.1,input$nn.cap.pred.2,
                                        input$nn.cap.pred.3,input$nn.cap.pred.4,
                                        input$nn.cap.pred.5,input$nn.cap.pred.6,
                                        input$nn.cap.pred.7,input$nn.cap.pred.8,
                                        input$nn.cap.pred.9,input$nn.cap.pred.10),
                     rl = rl.modelo.np(),
                     rlr = rlr.modelo.np(input$alpha.rlr.pred,input$switch.scale.rlr.pred,
                                         input$permitir.landa.pred, input$landa.pred))

      modelo.seleccionado.pn  <<- input$selectModelsPred
      modelo.nuevos <<- NULL
      predic.nuevos <<- NULL
      actualizar.pred.pn("")

      tryCatch({
        if(input$selectModelsPred == "ada" | input$selectModelsPred == "rl")
          if(num.categorias.pred.np() != 2)
            stop(tr("limitModel"), call. = FALSE)

        exe(codigo)
        actualizar.texto.modelo.pn(codigo)
      },
      error =  function(e){
        showNotification(paste0("Error: ", e), duration = 10, type = "error")
      },
      warning = function(w){
        if(input$selectModelsPred == "nn"){
          showNotification(paste0(tr("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
        }
      })
  })

  observeEvent(input$loadButtonNPred2,{
    codigo.carga <- code.carga( nombre.filas = input$rownameNPred2,
                                ruta = input$file3$datapath,
                                separador = input$sep.nPred2,
                                sep.decimal = input$dec.nPred2,
                                encabezado = input$headerNPred2,
                                d.o = "datos.prueba.completos",
                                d = "datos.prueba.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      varificar.datos.pn()
      if(ncol(datos.prueba.completos) <= 1) {
        showNotification(tr("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      codigo.na <- ""
      codigo.na <- paste0(code.NA(deleteNA = input$deleteNAnPred2,
                                  d.o = paste0("datos.prueba.completos")))

      datos.prueba.completos[,variable.predecir.pn] <<- NULL
      isolate(exe( codigo.na))
      datos.prueba.completos[,variable.predecir.pn] <<- NA
      code.trans.pn <<- gsub("datos.originales.completos", "datos.prueba.completos", code.trans.pn)
      code.trans.pn <<- gsub("datos.aprendizaje.completos", "datos.prueba.completos", code.trans.pn)
      exe(code.trans.pn)
      unificar.factores()
      actualizar.tabla.pn("contentsPred3")
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos.prueba.completos <<- NULL
      predic.nuevos <<- NULL
      return(NULL)
    })
  })

  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.landa.pred, {
    if (input$permitir.landa.pred) {
      shinyjs::enable("landa.pred")
    } else {
      shinyjs::disable("landa.pred")
    }
  })

  # PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

  len.report <- function(){
    length(env.report$codigo.reporte)
  }

  remove.report.elem <- function(id){
    insert.report(id, NULL)
  }

  insert.report <- function(id, content, interpretation =  TRUE){
    n <- len.report()
    if(is.null(content)){
      env.report$codigo.reporte[[n]][[id]] <<- content
    }else{
      env.report$codigo.reporte[[n]][[id]] <<- ifelse(interpretation, paste0(content,"\n\n#### Interpretación\n\n"), content)
    }
  }

  names.report <- function(){
    n <- len.report()
    names(env.report$codigo.reporte[[n]])
  }

  new.report <- function(){
    n <- len.report() + 1
    env.report$codigo.reporte[[n]] <<- list(datos.originales = datos.originales)
    env.report$codigo.reporte[[n]][["carga.datos"]] <<- paste0("\n# Carga de Datos (",input$file1$name,")",
                                                    "\n```{r}\ndatos.originales <<- codigo.reporte[[",n,"]]$datos.originales\n",
                                                    "datos <<- datos.originales\n```\n```{r}\nhead(datos)\n```\n```{r}\nstr(datos)\n```\n",
                                                    "```{r}\nIndicesM <<- list()\nareas <<- list()\nscores <<- list()\n```\n")
  }

  new.secction.report <- function(){
    n <- len.report() + 1
    env.report$codigo.reporte[[n]] <<- list()
  }

  observeEvent(input$principal, {
    if(input$principal == "reporte"){
      cod.report <<- def.reporte(titulo = input$textTitulo, nombre = input$textNombre, input)
      updateAceEditor(session, "fieldCodeReport", value = cod.report)
    }
  })

  observeEvent(input$textTitulo, {
    cod.report <<- str_replace(cod.report, "title: '.*'", paste0("title: '", input$textTitulo, "'"))
    updateAceEditor(session, "fieldCodeReport", value = cod.report )
  })

  # observeEvent(input$fieldCodeReport, {
  #   isolate(cod.report <<- input$fieldCodeReport)
  # })

  observeEvent(input$textNombre, {
    cod.report <<- str_replace(input$fieldCodeReport, "author: '.*'", paste0("author: '", input$textNombre, "'"))
    updateAceEditor(session, "fieldCodeReport", value = cod.report)
  })

  output$descargar <- downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      e <- options()$encoding
      options(encoding = enc) #Variable global
      write.table(cod.report,namermd,row.names=F,col.names=F,quote=F)
      options(encoding = e)

      files <- c(namermd, files)

      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite.cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(highlight = "tango"), envir = env.report)
      },
      message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
        shinyjs::html(id = "txtreport", html = salida.code)
      })

      recover.cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )

  # CAMBIAR IDIOMA ----------------------------------------------------------------------------------------------------------

  observeEvent(input$idioma, {

    actualizar.idioma()

    updatePlot$normal <- default.normal("datos", input$sel.normal, input$col.normal, tr("curvanormal"))
    updatePlot$dya.cat <- def.code.cat(variable = input$sel.distribucion.cat, titulox = tr("cantidadcasos"), tituloy = tr("categorias"))
    updatePlot$calc.normal <- default.calc.normal(labelsi = tr("positivo"),labelno=tr("negativo"),labelsin=tr("sinasimetria"))
    updatePlot$poder.pred <- plot.code.poder.pred(variable.predecir, label= tr("distrelvar"))
    updatePlot$poder.dens <- plot.numerico.dens(input$sel.density.poder,tr("denspodlab"))
    updatePlot$poder.cat <- plot.code.dist.porc(input$sel.distribucion.poder,variable.predecir, label=tr("distpodcat"))

    ejecutar.knn.mc()
    ejecutar.knn.ind()
    ejecutar.svm.mc()
    ejecutar.svm.ind()
    ejecutar.dt.mc()
    ejecutar.dt.ind()
    ejecutar.rf.mc()
    ejecutar.rf.ind()
    ejecutar.rl.mc()

    actualizar.selector.comparativa()
  })

  # TERMINA LA SESION -------------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    unlink("figure", recursive = T)
    recover.cat()
    stopApp()
  })

})
