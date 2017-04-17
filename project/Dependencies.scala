import sbt._

object Dependencies {

  lazy val testKit = Seq(
    akkaModule("testkit"),
    "org.scalatest" %% "scalatest" % "3.0.1",
    "org.scalactic" %% "scalactic" % "3.0.0",
    "org.scalacheck" %% "scalacheck" % "1.13.4",
    "net.databinder.dispatch" %% "dispatch-core" % "+",
    "org.mockito" % "mockito-all" % "1.10.19",
    "net.databinder.dispatch" %% "dispatch-core" % "+",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.5.+",
    "com.typesafe.akka" %% "akka-http-testkit" % "10.0.3"
  ) map (_ % "test")

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "18.+",
    "com.typesafe.play" %% "play-json" % "2.6.0-M1"
  )

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.4.14"
  lazy val akka = Seq("actor", "slf4j").map(akkaModule)

  lazy val p2p = Seq(
    "org.bitlet" % "weupnp" % "0.1.+"
  )

  lazy val db = Seq(
    "com.h2database" % "h2-mvstore" % "1.+"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.9+",
    "org.slf4j" % "slf4j-api" % "1.7.+"
  )

  def swaggerModule(module: String) = "io.swagger" % s"swagger-$module" % "1.+"
  lazy val http = Seq("scala-module_2.12", "core", "annotations", "models", "jaxrs").map(swaggerModule) ++ Seq(
    "com.chuusai" %% "shapeless" % "2.+",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.+"
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "1.+" % "test",
    "org.iq80.leveldb" % "leveldb" % "0.7",
    "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"
  )
}
