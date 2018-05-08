import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.4.19"

  def swaggerModule(module: String) = "io.swagger" % s"swagger-$module" % "1.5.16"

  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.0.9"

  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.22.Final"

  def kamonModule(v: String)(module: String) = "io.kamon" %% s"kamon-$module" % v

  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.1.0-alpha22"

  lazy val network = Seq("handler", "buffer", "codec").map(nettyModule) ++ Seq(
    "org.bitlet" % "weupnp" % "0.1.4",
    // Solves an issue with kamon-influxdb
    asyncHttpClient
  )

  lazy val testKit = scalatest ++ Seq(
    akkaModule("testkit"),
    "org.scalacheck" %% "scalacheck" % "1.13.5",
    "org.mockito" % "mockito-all" % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0",
    "org.iq80.leveldb" % "leveldb" % "0.9",
    akkaHttpModule("akka-http-testkit")
  )

  lazy val itKit = scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    "com.spotify" % "docker-client" % "8.9.0" classifier "shaded",
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.8.9",
    asyncHttpClient
  )

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "21.0",
    "com.typesafe.play" %% "play-json" % "2.6.2"
  )
  lazy val akka = Seq("actor", "slf4j").map(akkaModule)

  lazy val db = Seq(
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.slf4j" % "slf4j-api" % "1.7.25",
    "org.slf4j" % "jul-to-slf4j" % "1.7.25",
    "net.logstash.logback" % "logstash-logback-encoder" % "4.11"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs").map(swaggerModule) ++ Seq(
    "io.swagger" %% "swagger-scala-module" % "1.0.4",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.9.2",
    akkaHttpModule("akka-http")
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.4.18.1" % "test",
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val metrics = {
    Seq("core", "system-metrics").map(kamonModule("0.6.7")) ++
      Seq("akka-2.4", "influxdb").map(kamonModule("0.6.8")) ++
      Seq(
        "org.influxdb" % "influxdb-java" % "2.7",
        "io.kamon" %% "kamon-autoweave" % "0.6.5"
      )
  }.map(_.exclude("org.asynchttpclient", "async-http-client"))

  lazy val fp = Seq(
    "org.typelevel" %% "cats-core" % "1.0.1",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
  )
  lazy val meta = Seq("com.chuusai" %% "shapeless" % "2.3.3")
  lazy val monix = Def.setting(Seq("io.monix" %%% "monix" % "3.0.0-M3"))
  lazy val scodec = Def.setting(Seq("org.scodec" %%% "scodec-core" % "1.10.3"))
  lazy val fastparse = Def.setting(Seq("com.lihaoyi" %%% "fastparse" % "1.0.0"))
  lazy val ficus = Seq("com.iheart" %% "ficus" % "1.4.2")
  lazy val scorex = Seq(("org.scorexfoundation" %% "scrypto" % "2.0.4").exclude("org.slf4j", "slf4j-api"))
  lazy val commons_net = Seq("commons-net" % "commons-net" % "3.+")
  lazy val scalatest = Seq("org.scalatest" %% "scalatest" % "3.0.3")
  lazy val scalactic = Seq("org.scalactic" %% "scalactic" % "3.0.3")
  lazy val cats = Seq("org.typelevel" %% "cats-core" % "1.0.1")
  lazy val scalacheck = Seq(
    "org.scalacheck" %% "scalacheck" % "1.13.5",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
  )
  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.6"
}
