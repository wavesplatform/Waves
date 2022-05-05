enablePlugins(IntegrationTestsPlugin, sbtdocker.DockerPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/node-it")),
    dockerfile := NativeDockerfile(baseDirectory.value.getParentFile / "docker" / "Dockerfile"),
    buildOptions := BuildOptions(),
    dockerBuildArguments := Map(
      "ENABLE_GRPC"   -> "true",
      "WAVES_NETWORK" -> "custom"
    )
  )
)

val packageAll = taskKey[Unit]("build all packages")
docker := docker.dependsOn(LocalProject("root") / packageAll).value
