enablePlugins(IntegrationTestsPlugin, sbtdocker.DockerPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

inTask(docker)(
  Seq(
    imageNames   := Seq(ImageName("com.wavesplatform/node-it")),
    dockerfile   := NativeDockerfile(baseDirectory.value.getParentFile / "docker" / "Dockerfile"),
    buildOptions := BuildOptions()
  )
)

val buildTarballsForDocker = taskKey[Unit]("build all packages")
docker := docker.dependsOn(LocalProject("waves-node") / buildTarballsForDocker).value
