enablePlugins(sbtdocker.DockerPlugin)

ItSettings.settings
DockerSettings.dexSettings

description := "DEX integration tests"
libraryDependencies ++= Dependencies.itTest

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it")),
    DockerSettings.exposedPorts := Set(6886),
    DockerSettings.additionalFiles ++= Seq(
      (LocalProject("dex") / Universal / stage).value,
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "wallet"
    )
  ))
