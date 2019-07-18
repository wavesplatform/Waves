import WavesDockerKeys._

name := "blockchain-updates"

libraryDependencies += Dependencies.kafka

extensionClasses += "com.wavesplatform.events.BlockchainUpdates"

inConfig(Compile)(
  Seq(
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value
  ))

enablePlugins(RunApplicationSettings, WavesExtensionDockerPlugin, ExtensionPackaging)

docker := docker.dependsOn(LocalProject("node-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/blockchain-updates")),
    exposedPorts := Set(6886),
    additionalFiles ++= Seq(
      (LocalProject("blockchain-updates") / Universal / stage).value
    )
  ))
