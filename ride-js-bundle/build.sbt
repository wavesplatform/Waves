import org.scalajs.linker.interface.ModuleSplitStyle

scalaJSLinkerConfig ~= {
  _.withModuleKind(ModuleKind.ESModule)
   .withModuleSplitStyle(ModuleSplitStyle.SmallestModules)
}

Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "dist"
Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "dist"
