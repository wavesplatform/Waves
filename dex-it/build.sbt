import WavesDockerKeys._

enablePlugins(WavesExtensionDockerPlugin, ItTestPlugin)

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest

docker := docker.dependsOn(LocalProject("node-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it")),
    exposedPorts := Set(6886),
    additionalFiles ++= Seq(
      (LocalProject("dex") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))
