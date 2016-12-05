import com.typesafe.config.ConfigFactory

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

name in ThisBuild := "waves"

organization in ThisBuild := "com.wavesplatform"

version in ThisBuild := appConf.getString("version")

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild += "-target:jvm-1.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

val scorexVersion = "1.4.6"

libraryDependencies ++= Seq(
  "com.wavesplatform" %% "scorex-basics" % scorexVersion,
  "com.wavesplatform" %% "scorex-consensus" % scorexVersion,
  "com.wavesplatform" %% "scorex-transaction" % scorexVersion,
  "io.spray" %% "spray-testkit" % "1.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

fork in ThisBuild := true
fork in Test := false
parallelExecution in ThisBuild := false
