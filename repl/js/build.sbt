scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

Compile / fullOptJS / artifactPath := baseDirectory.value / "dist" / "repl.js"
