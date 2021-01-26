enablePlugins(IntegrationTestsPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

lazy val docker = taskKey[Unit]("build docker image")
docker := {
  val result = new ProcessBuilder()
    .command("/bin/bash", "-c", "./docker_build.sh && docker build -t testnode --build-arg WAVES_VERSION=current --build-arg ENABLE_GRPC=true docker")
    .directory(baseDirectory.value.getParentFile)
    .inheritIO()
    .start()
    .waitFor()

  require(result == 0, "Docker build error")
}
