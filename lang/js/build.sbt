scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

Compile / fullOptJS / artifactPath := baseDirectory.value / "dist" / "lang.js"
