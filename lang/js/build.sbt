scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

Compile / fullOptJS / artifactPath := fileConverter.value.toVirtualFile((baseDirectory.value / "dist" / "lang.js").toPath)
