name := "grpc-server"

libraryDependencies ++= Dependencies.grpc

inConfig(Compile)(Seq(
  PB.unpackDependencies := {
    val deps = PB.unpackDependencies.value
    val waves = PB.externalIncludePath.value / "waves"
    val grpc = PB.externalIncludePath.value / "waves" / "grpc"
    val targetGrpc = target.value / "protobuf" / "waves" / "grpc"
    IO.move(grpc, targetGrpc)
    // IO.delete(waves)
    deps.copy(files = deps.files.filter(_.exists()))
  },
  PB.protoSources += target.value / "protobuf",
  PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
))

enablePlugins(RunApplicationSettings, ExtensionPackaging)
