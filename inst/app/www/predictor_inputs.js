// ShinyWidgets
var exports4but = window.Shiny = window.Shiny || {};
var $escape4but = exports4but.$escape = function(val) {
  return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
};

// radioGroupButtons input binding
var radioGroupButtonsBinding = new Shiny.InputBinding();
$.extend(radioGroupButtonsBinding, {
  find: function(scope) {
    return $(scope).find('.radioGroupButtons');
  },
  getId: function(el) {
    return el.id;
  },
  getValue: function(el) {
    return $('input:radio[name="' + $escape4but(el.id) + '"]:checked').val();
  },
  setValue: function(el, value) {
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').prop('checked', true);
    $('input:radio[name="' + $escape4but(el.id) + '"]').parent().removeClass('active');
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').parent().addClass('active');
  },
  subscribe: function(el, callback) {
    $(el).on('change.radioGroupButtonsBinding', function (event) {
        callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.radioGroupButtonsBinding');
  },
  getState: function getState(el) {
      var $objs = $('input:radio[name="' + $escape4but(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
        label: this._getLabel($objs[i]) };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
    };
  },
  receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        $el.find('div.btn-group-container-sw').empty();
        $el.find('div.btn-group-container-sw').append(data.options);
      }

      if (data.hasOwnProperty('selected'))
        this.setValue(el, data.selected);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(data.label);

      $(el).trigger('change');
  }
});

Shiny.inputBindings.register(radioGroupButtonsBinding, 'shiny.radioGroupButtonsInput');

// LabelInput
Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]");
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
    }
  }
);

var promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, bayes: true , xgb: true, nn: true, rl : true, rlr: true};

nya_btn = true;
function nya_btn_fun(e) {
  nya_btn = nya_btn?($("#distribucion_numerica").click(),false):true;
}

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

function hide_element(id){
    $("a[data-value='"+id+"']").hide();
}
function show_element(id){
    $("a[data-value='"+id+"']").show();
}

window.addEventListener("load", function(event) {
  
  //Genera los modelos si se presiona el tab
 // $("a[href^='#shiny-tab-knn']").on('click',      function(e) {promidat_model_firt(e,"knn","knn_ui_1-runKnn")});
 // $("a[href^='#shiny-tab-svm']").on('click',      function(e) {promidat_model_firt(e,"svm","svm_ui_1-runSvm")});
 // $("a[href^='#shiny-tab-dt']").on('click',       function(e) {promidat_model_firt(e,"dt" ,"d_tree_ui_1-runDt")});
  //$("a[href^='#shiny-tab-rf']").on('click',       function(e) {promidat_model_firt(e,"rf", "r_forest_ui_1-runRf")});
 // $("a[href^='#shiny-tab-xgb']").on('click',      function(e) {promidat_model_firt(e,"xgb","xgboosting_ui_1-runXgb")});
 // $("a[href^='#shiny-tab-boosting']").on('click', function(e) {promidat_model_firt(e,"boosting","boosting_ui_1-runBoosting")});
  //$("a[href^='#shiny-tab-bayes']").on('click',    function(e) {promidat_model_firt(e,"bayes","bayes_ui_1-runBayes")});
 // $("a[href^='#shiny-tab-nn']").on('click',       function(e) {promidat_model_firt(e,"nn","neural_net_ui_1-runNn")});
  //$("a[href$='#shiny-tab-rl']").on('click',       function(e) {promidat_model_firt(e,"rl","l_regression_ui_1-runRl")});
 // $("a[href^='#shiny-tab-rlr']").on('click',      function(e) {promidat_model_firt(e,"rlr","penalized_l_r_ui_1-runRlr")});

    //Botones de Wizard
  $("#ind_nuevos_ui_1-nuevosnext").on('click',function(e) {$("#ind_nuevos_ui_1-predecirPromidat").click()});
  $("#ind_nuevos_ui_1-transButton").on('click',function(e) {$("#ind_nuevos_ui_1-PredNuevosBttnModelo").click()});
  //$("#ind_nuevos_ui_1-cargarnext").on('click',function(e) {$("#ind_nuevos_ui_1-transButton").click()});

  /* Los modelos se vuelven ejecutar al ser seleccionados una vez se haga una segmentacion diferente
  $($($($("#tabDyA").next().children()[2]).children()[4]).children()[2]).on('click', nya_btn_fun);*/
  
  $("#carga_datos_ui_1-segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, bayes: true, xgb: true, nn: true, rl : true, rlr: true};
  });

});


function eliminar_tabs_extras(){
  $("ul#BoxNormal li")[2].remove();
  $("ul#BoxDisp li")[1].remove();
  $("ul#tabDyA li")[2].remove();
  $("ul#tabCor li").last().remove();
  $("ul#BoxKnn li").last().remove();
  $("ul#BoxDt li").last().remove();
  $("ul#BoxRf li").last().remove();
  $("ul#BoxB li").last().remove();
  $("ul#BoxSvm li").last().remove();
  $("ul#BoxCom li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxBayes li").last().remove();
  $("ul#BoxXgb li").last().remove();
  $("ul#BoxNn li").last().remove();
  $("ul#BoxRl li").last().remove();
  $("ul#BoxRlr li").last().remove();
}



