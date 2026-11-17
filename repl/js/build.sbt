scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

// Links straight into the @waves/ride-js package: repl-js depends on lang-js, so its output carries both export sets.
Compile / fastOptJS / artifactPath := fileConverter.value.toVirtualFile(((ThisBuild / baseDirectory).value / "ride-js-bundle" / "scalajs" / "ride-scalajs.js").toPath)
Compile / fullOptJS / artifactPath := fileConverter.value.toVirtualFile(((ThisBuild / baseDirectory).value / "ride-js-bundle" / "scalajs" / "ride-scalajs.js").toPath)
