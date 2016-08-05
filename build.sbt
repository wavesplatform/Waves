import com.typesafe.config.ConfigFactory

organization := "com.wavesplatform"

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

name := "waves"

version := appConf.getString("version")

scalaVersion := "2.11.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

val modulesVersion = "1.3.3-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.wavesplatform" %% "scorex-basics" % modulesVersion,
  "com.wavesplatform" %% "scorex-consensus" % modulesVersion,
  "com.wavesplatform" %% "scorex-transaction" % modulesVersion,
  "io.spray" %% "spray-testkit" % "1.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)


//assembly settings
assemblyJarName in assembly := "waves.jar"

test in assembly := {}

fork in ThisBuild := true
parallelExecution in ThisBuild := false

mainClass in assembly := Some("com.wavesplatform.Application")

assemblyMergeStrategy in assembly := {
  case "application.conf" => MergeStrategy.concat
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}