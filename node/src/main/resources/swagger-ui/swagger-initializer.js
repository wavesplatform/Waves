window.onload = function() {
  //<editor-fold desc="Changeable Configuration Block">

  // https://github.com/swagger-api/swagger-ui/blob/master/docs/usage/configuration.md
  // the following lines will be replaced by docker/configurator, when it runs in a docker-container
  window.ui = SwaggerUIBundle({
    url: "/api-docs/openapi.yaml",
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [
      SwaggerUIBundle.presets.apis,
      SwaggerUIStandalonePreset
    ],
    plugins: [
      SwaggerUIBundle.plugins.DownloadUrl
    ],
    layout: "StandaloneLayout",
    tagsSorter: "alpha",
    operationsSorter: "alpha"
  });

  //</editor-fold>
};
