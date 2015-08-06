import com.typesafe.config._

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

organization := "org.consensusresearch"

name := appConf.getString("product")

version := appConf.getString("version")

scalaVersion := "2.11.7"

resolvers ++= Seq("Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++= Seq(
  "org.mapdb" % "mapdb" % "1.+",
  "com.typesafe.play" %% "play-json" % "2.+",
  "com.typesafe.akka" %% "akka-actor" % "2.+",
  "io.spray" %% "spray-routing" % "1.+",
  "io.spray" %% "spray-can" % "1.+",
  "io.spray" %% "spray-http" % "1.+",
  "io.spray" %% "spray-httpx" % "1.+",
  "io.spray" %% "spray-util" % "1.+",
  "com.google.guava" % "guava" % "15.+",
  "commons-net" % "commons-net" % "3.+",
  "ch.qos.logback" % "logback-classic" % "1.+",
  "ch.qos.logback" % "logback-core" % "1.+",

  //dependencies for testing:
  "io.spray" %% "spray-testkit" % "1.+" % "test",
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test"
)

javaOptions ++= Seq(
  "-server"
)

//assembly settings
assemblyJarName in assembly := "scorex.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.Start")