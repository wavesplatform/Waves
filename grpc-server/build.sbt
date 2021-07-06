import sbt.nio.file.FileAttributes

name := "waves-grpc-server"

libraryDependencies ++= Dependencies.grpc

extensionClasses ++= Seq(
  "com.wavesplatform.api.grpc.GRPCServerExtension",
  "com.wavesplatform.events.BlockchainUpdates"
)

inConfig(Compile)(
  Seq(
    Compile / PB.protoSources := Seq(PB.externalIncludePath.value),
    PB.generate / includeFilter := new SimpleFileFilter(
      (f: File) =>
        ((** / "waves" / "node" / "grpc" / ** / "*.proto") || (** / "waves" / "events" / ** / "*.proto"))
          .accept(f.toPath, FileAttributes(f.toPath).getOrElse(FileAttributes.NonExistent))
    ),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)

enablePlugins(RunApplicationSettings, ExtensionPackaging)

Debian / debianControlFile := {
  val generatedFile = (Debian / debianControlFile).value
  IO.append(generatedFile, s"""Conflicts: grpc-server${network.value.packageSuffix}
      |Replaces: grpc-server${network.value.packageSuffix}
      |""".stripMargin)
  generatedFile
}
