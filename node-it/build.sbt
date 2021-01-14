enablePlugins(IntegrationTestsPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

lazy val docker = taskKey[Unit]("build docker image")
docker := {
  val command = "docker build -t testnode --build-arg WAVES_VERSION=current --build-arg ENABLE_GRPC=true -f docker/Dockerfile ."
  val result = new ProcessBuilder()
    .command(command.split(" "): _*)
    .directory(baseDirectory.value.getParentFile)
    .inheritIO()
    .start()
    .waitFor()

  require(result == 0, "Docker build error")
}
