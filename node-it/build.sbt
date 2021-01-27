enablePlugins(IntegrationTestsPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

lazy val docker = taskKey[Unit]("build docker image")
docker := {
  val dockerDir = new File(baseDirectory.value.getParentFile, "docker")

  val nodeDebFile = (packageBin in Debian in LocalProject("node")).value
  val grpcDebFile = (packageBin in Debian in LocalProject("grpc-server")).value
  IO.copyFile(nodeDebFile, new File(dockerDir, "target/waves.deb"))
  IO.copyFile(grpcDebFile, new File(dockerDir, "target/grpc-server.deb"))

  val result = new ProcessBuilder()
    .command("docker build -t testnode --build-arg ENABLE_GRPC=true .".split(' '): _*)
    .directory(dockerDir)
    .inheritIO()
    .start()
    .waitFor()

  require(result == 0, "Docker build error")
}
