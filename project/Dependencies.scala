import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.4.17"
  def swaggerModule(module: String) = "io.swagger" % s"swagger-$module" % "1.5.13"
  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.0.5"

  lazy val scalatest = Seq(
    "org.scalatest" %% "scalatest" % "3.0.1",
    "org.scalactic" %% "scalactic" % "3.0.0"
  )

  lazy val testKit = (scalatest ++ Seq(
    akkaModule("testkit"),
    "org.scalatest" %% "scalatest" % "3.0.1",
    "org.scalactic" %% "scalactic" % "3.0.0",
    "org.scalacheck" %% "scalacheck" % "1.13.4",
    "org.mockito" % "mockito-all" % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0",
    "net.databinder.dispatch" % "dispatch-core_2.12" % "0.12.0",
    akkaHttpModule("akka-http-testkit")
  )) map (_ % "test")

  lazy val itKit = (scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    "com.spotify" % "docker-client" % "8.1.2" classifier "shaded",
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.8.7",
    "org.asynchttpclient" % "async-http-client" % "2.0.31"
  )) map (_ % "it,test")

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "21.0",
    "com.typesafe.play" %% "play-json" % "2.6.0-M1"
  )
  lazy val akka = Seq("actor", "slf4j").map(akkaModule)

  lazy val p2p = Seq(
    "org.bitlet" % "weupnp" % "0.1.4"
  )

  lazy val db = Seq(
    "com.h2database" % "h2-mvstore" % "1.4.194"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.9",
    "org.slf4j" % "slf4j-api" % "1.7.25"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs").map(swaggerModule) ++ Seq(
    "io.swagger" %% "swagger-scala-module" % "1.0.3",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.9.1",
    akkaHttpModule("akka-http")
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.4.17.3" % "test",
    "org.iq80.leveldb" % "leveldb" % "0.7",
    "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"
  )
}
