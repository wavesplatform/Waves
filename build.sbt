import com.typesafe.config.ConfigFactory

organization := "org.consensusresearch"

val appConf = ConfigFactory.parseFile(new File("src/main/resources/waves.conf")).resolve().getConfig("app")

name := "waves"

version := appConf.getString("version")

scalaVersion := "2.11.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.consensusresearch" %% "scorex-basics" % "1.3.0-M1",
  "org.consensusresearch" %% "scorex-consensus" % "1.3.0-M1",
  "org.consensusresearch" %% "scorex-transaction" % "1.3.0-M1",
  "io.spray" %% "spray-testkit" % "1.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

//assembly settings
assemblyJarName in assembly := "waves.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.waves.Application")