// Changes the document title
const TitlePlugin = function(system) {
  return {
    statePlugins: {
      spec: {
        wrapSelectors: {
          specJson: (oriSelector) => (...args) => {
            const result = oriSelector(...args);

            try {
              const js = result?.toJS?.();
              const title = js?.info?.title;

              if (title) document.title = title;
            } catch (e) { }

            return result;
          }
        }
      }
    }
  };
};

window.onload = function() {
  //<editor-fold desc="Changeable Configuration Block">

  // https://github.com/swagger-api/swagger-ui/blob/master/docs/usage/configuration.md
  // the following lines will be replaced by docker/configurator, when it runs in a docker-container
  window.ui = SwaggerUIBundle({
    url: "/api-docs/openapi.yaml",
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [
      SwaggerUIBundle.presets.apis
    ],
    plugins: [
      SwaggerUIBundle.plugins.DownloadUrl,
      TitlePlugin
    ],
    // Hide "Explore" button and input field. StandaloneLayout shows them
    // See https://github.com/swagger-api/swagger-ui/blob/master/src/standalone/presets/standalone/index.js
    layout: "BaseLayout",
    tagsSorter: "alpha",
    operationsSorter: "alpha",
    tryItOutEnabled: true // Always show "Execute" button
  });

  //</editor-fold>
};
