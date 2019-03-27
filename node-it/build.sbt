enablePlugins(sbtdocker.DockerPlugin)

ItSettings.settings
DockerSettings.settings

description := "NODE integration tests"
libraryDependencies ++= Dependencies.itTest

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/node-it")),
    DockerSettings.exposedPorts := Set(6863, 6869), // NetworkApi, RestApi
    DockerSettings.additionalFiles ++= (LocalProject("node") / Universal / stage).value +: Seq(
      (Test / resourceDirectory).value / "template.conf",
      (Test / sourceDirectory).value / "container" / "start-waves.sh"
    )
  ))
