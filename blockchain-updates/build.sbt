import WavesDockerKeys._

name := "blockchain-updates"

libraryDependencies ++= Dependencies.grpc

extensionClasses += "com.wavesplatform.events.BlockchainUpdates"

inConfig(Compile)(
  Seq(
    PB.protoSources in Compile := Seq(PB.externalIncludePath.value, sourceDirectory.value / "protobuf"),
    includeFilter in PB.generate := new SimpleFileFilter(
      (f: File) => {
        val name = f.getName
        name == "blockchain_updates.proto" || name == "events_temp.proto" || (name.endsWith(".proto") && f.getParent.contains("waves") && !name
          .endsWith("events.proto"))
      }
    ),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  )
)

enablePlugins(RunApplicationSettings, WavesExtensionDockerPlugin, ExtensionPackaging)

docker := docker.dependsOn(LocalProject("node-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/blockchain-updates")),
    exposedPorts := Set(6886),
    additionalFiles ++= Seq(
      (LocalProject("blockchain-updates") / Universal / stage).value
    )
  )
)
