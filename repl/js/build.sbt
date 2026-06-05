scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

// Link the combined RIDE compiler+repl module straight into the @waves/ride-js bundle
// (no copy step). repl-js is the superset that re-exports every @JSExportTopLevel from
// lang-js as well, so this single artifact is exactly what the npm package ships.
Compile / fastOptJS / artifactPath := (ThisBuild / baseDirectory).value / "ride-js-bundle" / "scalajs" / "ride-scalajs.js"
Compile / fullOptJS / artifactPath := (ThisBuild / baseDirectory).value / "ride-js-bundle" / "scalajs" / "ride-scalajs.js"
