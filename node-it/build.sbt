enablePlugins(IntegrationTestsPlugin, sbtdocker.DockerPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

imageNames in docker := Seq(ImageName("com.wavesplatform/node-it"))

dockerfile in docker := NativeDockerfile(baseDirectory.value.getParentFile / "docker" / "Dockerfile")

buildOptions in docker := BuildOptions()

dockerBuildArguments := Map(
  "ENABLE_GRPC" -> "true"
)

lazy val packageDeb = taskKey[Unit]("Package DEB packages for docker build")

packageDeb := {
  val targetDir = baseDirectory.value.getParentFile / "docker" / "target"
  IO.copyFile(
    (LocalProject("node") / Debian / packageBin).value,
    new File(targetDir, "waves.deb")
  )

  IO.copyFile(
    (LocalProject("grpc-server") / Debian / packageBin).value,
    new File(targetDir, "grpc-server.deb")
  )
}

docker := docker.dependsOn(packageDeb).value
