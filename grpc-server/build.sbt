name := "grpc-server"

libraryDependencies ++= Dependencies.grpc

extensionClasses += "com.wavesplatform.api.grpc.GRPCServerExtension"

inConfig(Compile)(Seq(
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
))

enablePlugins(RunApplicationSettings, ExtensionPackaging)
