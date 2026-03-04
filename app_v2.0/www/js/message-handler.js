// Existing server-driven message handler (kept for compatibility).
Shiny.addCustomMessageHandler("testmessage", function(message) {
  alert(JSON.stringify(message));
});

(function() {
  var spanishMap = {
    "Reference sample": "Muestra de referencia",
    "General Description": "Descripción general",
    "Description of Distances": "Descripción de distancias",
    "Sex estimation": "Estimación de sexo",
    "Results": "Resultados",
    "Anatomical Zone": "Zona anatómica",
    "Maxillary": "Maxilar",
    "Mandibular": "Mandibular",
    "Abstract": "Resumen",
    "Diagram": "Diagrama",
    "Sex": "Sexo",
    "Age - Sex": "Edad - sexo",
    "Maxillary area - Sex": "Área maxilar - sexo",
    "Mandibular area - Sex": "Área mandibular - sexo",
    "Intercanine Distance": "Distancia intercanina",
    "Mesiodistal Right": "Mesiodistal derecha",
    "Mesiodistal Left": "Mesiodistal izquierda",
    "Number of measurements": "Número de mediciones",
    "Select measurement": "Seleccionar medición",
    "Visualization of predicted values": "Visualización de valores predichos",
    "Precision-Recall curve": "Curva precisión-recall"
  };

  var originals = new WeakMap();

  function applyTextTranslation(node, lang) {
    if (!originals.has(node)) {
      originals.set(node, node.textContent);
    }

    var original = originals.get(node);
    var trimmed = original.trim();

    if (!trimmed) {
      return;
    }

    if (lang === 'es' && spanishMap[trimmed]) {
      node.textContent = original.replace(trimmed, spanishMap[trimmed]);
      return;
    }

    node.textContent = original;
  }

  function translateByDataI18n(lang) {
    document.querySelectorAll('[data-i18n]').forEach(function(el) {
      var key = el.getAttribute('data-i18n');

      if (key === 'language_label') {
        el.textContent = lang === 'es' ? 'Idioma' : 'Language';
      }
    });
  }

  function translateDom(lang) {
    var navbarBrand = document.querySelector('.navbar-brand');
    if (navbarBrand) {
      navbarBrand.textContent = lang === 'es' ? 'Distancia intercanina' : 'Intercanine Distance';
    }

    translateByDataI18n(lang);

    var walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null, false);
    var node;

    while ((node = walker.nextNode())) {
      applyTextTranslation(node, lang);
    }
  }

  function applyLanguage(lang) {
    document.documentElement.setAttribute('lang', lang);
    translateDom(lang);
  }

  function initializeLanguageSelector() {
    var selector = document.getElementById('language_selector');
    if (!selector) {
      return;
    }

    applyLanguage(selector.value || 'es');

    selector.addEventListener('change', function(event) {
      applyLanguage(event.target.value);
    });

    var observer = new MutationObserver(function() {
      applyLanguage(selector.value || 'es');
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  document.addEventListener('DOMContentLoaded', initializeLanguageSelector);
})();
