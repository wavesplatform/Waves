import com.typesafe.config.ConfigFactory

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

name in ThisBuild := "waves"

organization in ThisBuild := "com.wavesplatform"

version in ThisBuild := appConf.getString("version")

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild += "-target:jvm-1.8"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

val scorexVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.wavesplatform" %% "scorex-basics" % scorexVersion,
  "com.wavesplatform" %% "scorex-consensus" % scorexVersion,
  "com.wavesplatform" %% "scorex-transaction" % scorexVersion,
  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalactic" %% "scalactic" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test"
)

val akkaV       = "2.4.14"
lazy val addAkkaLibs = Seq(
  "com.typesafe.akka" %% "akka-persistence" % akkaV,
  "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.+" % "test",
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8")

libraryDependencies ++= addAkkaLibs

fork in ThisBuild := true
fork in Test := true
parallelExecution in ThisBuild := false