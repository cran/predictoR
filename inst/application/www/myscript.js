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

shinyjs.init = function() {
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
  $("[data-widget='left']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-left")
  });
  $("[data-widget='centerleft']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centerleft")
  });
  $("[data-widget='centeright']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centeright")
  });
  $("[data-widget='center']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-center")
  });
  $("[data-widget='right']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-left");
    a.toggleClass("box-option-open-right")
  });

  eliminar_tabs_extras()
}

Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]")
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
      if(message.ids[i] == "categorico" || message.ids[i] == "numerico" || message.ids[i] == "disyuntivo"){
        $("option[value='"+message.ids[i]+"']").text(message.values[i])
      }
    }
  }
);

 window.addEventListener("load", function(event) {
  /* Al cargarse la página  se pone el titulo */
  $("header").find("nav").append('<span class="header-title"> <i>predicto</i>R </span>');

  /* Los modelos se vuelven ejecutar al ser seleccionados una vez se haga una segmentacion diferente*/
  $("a[href^='#shiny-tab-knn']").on('click', function(e) {promidat_model_firt(e,"knn","runKnn")});
  $("a[href^='#shiny-tab-dt']").on('click', function(e) {promidat_model_firt(e,"dt","runDt")});
  $("a[href^='#shiny-tab-rf']").on('click', function(e) {promidat_model_firt(e,"rf","runRf")});
  $("a[href^='#shiny-tab-boosting']").on('click', function(e) {promidat_model_firt(e,"boosting","runBoosting")});
  $("a[href^='#shiny-tab-svm']").on('click', function(e) {promidat_model_firt(e,"svm","runSvm")});
  $("a[href^='#shiny-tab-bayes']").on('click', function(e) {promidat_model_firt(e,"bayes","runBayes")});
  $("a[href^='#shiny-tab-xgb']").on('click', function(e) {promidat_model_firt(e,"xgb","runXgb")});
  $("a[href^='#shiny-tab-nn']").on('click', function(e) {promidat_model_firt(e,"nn","runNn")});
  $("a[href$='#shiny-tab-rl']").on('click', function(e) {promidat_model_firt(e,"rl","runRl")});
  $("a[href^='#shiny-tab-rlr']").on('click', function(e) {promidat_model_firt(e,"rlr","runRlr")});
  $("a[data-value='predicModelo']").on('click',function(e) {$("#predecirPromidat").click()});
  $($($($("#tabDyA").next().children()[2]).children()[4]).children()[2]).on('click', nya_btn_fun);
  $("#segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, bayes: true, xgb: true, nn: true, rl : true, rlr: true};
  });

});

/* ------------------------  */

//ShinyWidgets
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
