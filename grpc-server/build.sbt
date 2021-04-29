import sbt.nio.file.FileAttributes

name := "grpc-server"

libraryDependencies ++= Dependencies.grpc

extensionClasses ++= Seq(
  "com.wavesplatform.api.grpc.GRPCServerExtension",
  "com.wavesplatform.events.BlockchainUpdates"
)

inConfig(Compile)(
  Seq(
    PB.protoSources in Compile := Seq(PB.externalIncludePath.value),
    includeFilter in PB.generate := new SimpleFileFilter(
      (f: File) =>
        ((** / "waves" / "node" / "grpc" / ** / "*.proto") || (** / "waves" / "events" / ** / "*.proto"))
          .accept(f.toPath, FileAttributes(f.toPath).getOrElse(FileAttributes.NonExistent))
    ),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)

ExtensionPackaging.setPackageName(Def.setting("waves-node-grpc-server"))

enablePlugins(RunApplicationSettings, ExtensionPackaging)
