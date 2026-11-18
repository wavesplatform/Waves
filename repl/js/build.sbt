scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

// fullOptJS links straight into the @waves/ride-js package: repl-js depends on lang-js, so its output carries both
// export sets. fastOptJS stays under target/, so a dev link never replaces the optimised artifact the package ships.
Compile / fullOptJS / artifactPath := fileConverter.value.toVirtualFile(((ThisBuild / baseDirectory).value / "ride-js-bundle" / "scalajs" / "ride-scalajs.js").toPath)
