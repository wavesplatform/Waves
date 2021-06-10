scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.CommonJSModule)
}

libraryDependencies ++= Dependencies.circe.value
