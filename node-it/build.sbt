enablePlugins(IntegrationTestsPlugin, sbtdocker.DockerPlugin)

description := "NODE integration tests"
libraryDependencies ++= Dependencies.it

imageNames in docker := Seq(ImageName("com.wavesplatform/node-it"))

dockerfile in docker := NativeDockerfile(baseDirectory.value.getParentFile / "docker" / "Dockerfile")

buildOptions in docker := BuildOptions()

dockerBuildArguments := Map(
  "ENABLE_GRPC" -> "true"
)

val packageAll = taskKey[Unit]("build all packages")
docker := docker.dependsOn(packageAll in LocalProject("root")).value

