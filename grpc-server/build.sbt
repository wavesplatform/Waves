import WavesDockerKeys.{additionalFiles, exposedPorts}

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
        Seq(
          ** / "waves" / "node" / "grpc" / ** / "*.proto",
          ** / "waves" / "events" / ** / "*.proto"
        ).exists(_.matches(f.toPath))
    ),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)

enablePlugins(RunApplicationSettings, ExtensionPackaging, WavesExtensionDockerPlugin)

docker := docker.dependsOn(LocalProject("node-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/blockchain-updates")),
    exposedPorts := Set(6880, 6881),
    additionalFiles ++= Seq(
      (LocalProject("grpc-server") / Universal / stage).value
    )
  )
)
