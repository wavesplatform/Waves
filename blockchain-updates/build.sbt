name := "blockchain-updates"

libraryDependencies += "org.apache.kafka" %% "kafka" % "2.1.0"

inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
))

enablePlugins(RunApplicationSettings, ExtensionPackaging)