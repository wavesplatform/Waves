import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.4.19"
  def swaggerModule(module: String) = "io.swagger" % s"swagger-$module" % "1.5.16"
  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.0.9"
  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.13.Final"
  def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "0.6.7"

  lazy val network = Seq("handler", "buffer", "codec").map(nettyModule) ++ Seq(
    "org.bitlet" % "weupnp" % "0.1.4"
  )

  lazy val scalatest = Seq(
    "org.scalatest" %% "scalatest" % "3.0.3",
    "org.scalactic" %% "scalactic" % "3.0.3"
  )

  lazy val testKit = (scalatest ++ Seq(
    akkaModule("testkit"),
    "org.scalacheck" %% "scalacheck" % "1.13.5",
    "org.mockito" % "mockito-all" % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0",
    akkaHttpModule("akka-http-testkit")
  )) map (_ % "test")

  lazy val itKit = (scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    "com.spotify" % "docker-client" % "8.8.2" classifier "shaded",
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.8.9",
    "org.asynchttpclient" % "async-http-client" % "2.1.0-alpha22"
  )) map (_ % "it,test")

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "21.0",
    "com.typesafe.play" %% "play-json" % "2.6.2"
  )
  lazy val akka = Seq("actor", "slf4j").map(akkaModule)

  lazy val db = Seq(
    "com.h2database" % "h2-mvstore" % "1.4.196"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.slf4j" % "slf4j-api" % "1.7.25"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs").map(swaggerModule) ++ Seq(
    "io.swagger" %% "swagger-scala-module" % "1.0.4",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.9.2",
    akkaHttpModule("akka-http")
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.4.18.1" % "test",
    "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"
  )

  lazy val metrics = Seq("core", "statsd", "system-metrics").map(kamonModule) ++ Seq(
    "org.influxdb" % "influxdb-java" % "2.7"
  )

  lazy val fp = Seq(
    "org.typelevel" %% "cats-core" % "0.9.0"
  )
}
