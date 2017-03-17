import com.typesafe.config.ConfigFactory
import sbt.Keys._

val appConf = ConfigFactory.parseFile(new File("src/main/resources/reference.conf")).resolve().getConfig("app")

inThisBuild(Seq(
  organization in ThisBuild := "com.wavesplatform",
  name := "waves",
  version := appConf.getString("version"),
  scalaVersion := "2.12.1"
))

scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128")

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++=
  Dependencies.db ++
  Dependencies.http ++
  Dependencies.akka ++
  Dependencies.serialization ++
  Dependencies.testKit ++
  Dependencies.itKit ++
  Dependencies.logging ++
  Dependencies.matcher ++
  Dependencies.p2p ++
  Seq(
    "com.iheart" %% "ficus" % "1.4.0",
    "org.scorexfoundation" %% "scrypto" % "1.2.0",
    "commons-net" % "commons-net" % "3.+"
  )

inConfig(Test)(Seq(
  javaOptions += "-Dlogback.configurationFile=logback-sbt.xml",
  fork := true,
  parallelExecution := false,
  testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports")
))

Defaults.itSettings

configs(IntegrationTest)

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

test in assembly := {}

dockerfile in docker := {
  val configTemplate = (resourceDirectory in IntegrationTest).value / "template.conf"
  val startWaves = (sourceDirectory in IntegrationTest).value / "container" / "start-waves.sh"

  new Dockerfile {
    from("anapsix/alpine-java:8_server-jre")
    add(assembly.value, "/opt/waves/waves.jar")
    add(Seq(configTemplate, startWaves), "/opt/waves/")
    run("chmod", "+x", "/opt/waves/start-waves.sh")
    entryPoint("/opt/waves/start-waves.sh")
  }
}

inConfig(IntegrationTest)(Seq(
  fork := true,
  parallelExecution := false,
  javaOptions ++= Seq(
    s"-Ddocker.imageId=${docker.value.id}",
    "-Dlogback.configurationFile=logback-it.xml"
  ),
  test <<= test.dependsOn(docker),
  testOptions += Tests.Filter(_.endsWith("Suite"))
))
