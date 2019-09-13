name := "grpc-server"

libraryDependencies ++= Dependencies.grpc

extensionClasses += "com.wavesplatform.api.grpc.GRPCServerExtension"

inConfig(Compile)(Seq(
  PB.protoSources in Compile := Seq(PB.externalIncludePath.value),
  includeFilter in PB.generate := new SimpleFileFilter((f: File) => f.getName.endsWith(".proto") && f.getParent.endsWith("waves/node/grpc")),
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
))

enablePlugins(RunApplicationSettings, ExtensionPackaging)
