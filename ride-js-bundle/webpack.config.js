const path = require("path");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

// Browser bundle. The entry (dist/index.js) requires the single combined ScalaJS
// artifact (scalajs/ride-scalajs.js) exactly once, so it is inlined once here too.
module.exports = {
  mode: "production",
  entry: "./dist/index.js",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "ride.min.js",
    library: "RideJS"
  },
  plugins: [new NodePolyfillPlugin()]
};
