scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

Compile / fullOptJS / artifactPath := fileConverter.value.toVirtualFile((baseDirectory.value / "dist" / "repl.js").toPath)
