import com.typesafe.config.ConfigFactory

organization := "org.consensusresearch"

val appConf = ConfigFactory.parseFile(new File("src/main/resources/waves.conf")).resolve().getConfig("app")

name := "waves"

version := appConf.getString("version")

scalaVersion := "2.11.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.consensusresearch" %% "scorex-basics" % "1.2.+",
  "org.consensusresearch" %% "scorex-consensus" % "1.2.+",
  "org.consensusresearch" %% "scorex-transaction" % "1.2.+"
)

//assembly settings
assemblyJarName in assembly := "waves.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.waves.Application")