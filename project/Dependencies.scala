import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.4.19"

  def swaggerModule(module: String) = ("io.swagger" % s"swagger-$module" % "1.5.16").exclude("com.google.guava", "guava")

  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.0.9"

  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.24.Final"

  def kamonModule(module: String, v: String) = "io.kamon" %% s"kamon-$module" % v

  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.4.7"

  lazy val network = Seq("handler", "buffer", "codec").map(nettyModule) ++ Seq(
    "org.bitlet" % "weupnp" % "0.1.4",
    // Solves an issue with kamon-influxdb
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val testKit = scalatest ++ Seq(
    akkaModule("testkit"),
    "org.scalacheck" %% "scalacheck"                  % "1.13.5",
    "org.mockito"    % "mockito-all"                  % "1.10.19",
    "org.scalamock"  %% "scalamock-scalatest-support" % "3.6.0",
    ("org.iq80.leveldb" % "leveldb" % "0.9").exclude("com.google.guava", "guava"),
    akkaHttpModule("akka-http-testkit")
  )

  lazy val itKit = scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.11.3").classifier("shaded").exclude("com.google.guava", "guava"),
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.5",
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val serialization = Seq(
    "com.google.guava"  % "guava"      % "21.0",
    "com.typesafe.play" %% "play-json" % "2.6.9"
  )
  lazy val akka = Seq("actor", "slf4j").map(akkaModule)

  lazy val db = Seq(
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val logging = Seq(
    "ch.qos.logback"       % "logback-classic"          % "1.2.3",
    "org.slf4j"            % "slf4j-api"                % "1.7.25",
    "org.slf4j"            % "jul-to-slf4j"             % "1.7.25",
    "net.logstash.logback" % "logstash-logback-encoder" % "4.11"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs").map(swaggerModule) ++ Seq(
    "io.swagger"                   %% "swagger-scala-module" % "1.0.4",
    "com.github.swagger-akka-http" %% "swagger-akka-http"    % "0.9.2",
    akkaHttpModule("akka-http")
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    akkaModule("persistence-tck") % "test",
    "com.github.dnvriend"         %% "akka-persistence-inmemory" % "2.4.18.1" % "test",
    "org.ethereum"                % "leveldbjni-all" % "1.18.3"
  )

  lazy val metrics = Seq(
    kamonModule("core", "1.1.3"),
    kamonModule("system-metrics", "1.0.0"),
    kamonModule("akka-2.4", "1.1.1"),
    kamonModule("influxdb", "1.0.1"),
    "org.influxdb" % "influxdb-java" % "2.11"
  ).map(_.exclude("org.asynchttpclient", "async-http-client"))

  lazy val fp = Seq(
    "org.typelevel"       %% "cats-core"       % "1.1.0",
    "org.typelevel"       %% "cats-mtl-core"   % "0.2.1",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
  )
  lazy val meta = Seq("com.chuusai" %% "shapeless" % "2.3.3")
  lazy val monix = Def.setting(
    Seq(
      // exclusion and explicit dependency can likely be removed when monix 3 is released
      ("io.monix" %%% "monix" % "3.0.0-RC1").exclude("org.typelevel", "cats-effect_2.12"),
      "org.typelevel" %%% "cats-effect" % "0.10.1"
    ))
  lazy val scodec    = Def.setting(Seq("org.scodec" %%% "scodec-core" % "1.10.3"))
  lazy val fastparse = Def.setting(Seq("com.lihaoyi" %%% "fastparse" % "1.0.0", "org.bykn" %%% "fastparse-cats-core" % "0.1.0"))
  lazy val ficus     = Seq("com.iheart" %% "ficus" % "1.4.2")
  lazy val scorex = Seq(
    "org.scorexfoundation" %% "scrypto" % "2.0.4" excludeAll (
      ExclusionRule("org.slf4j", "slf4j-api"),
      ExclusionRule("com.google.guava", "guava")
    ))
  lazy val commons_net = Seq("commons-net"   % "commons-net" % "3.+")
  lazy val scalatest   = Seq("org.scalatest" %% "scalatest"  % "3.0.3")
  lazy val scalactic   = Seq("org.scalactic" %% "scalactic"  % "3.0.3")
  lazy val cats        = Seq("org.typelevel" %% "cats-core"  % "1.1.0")
  lazy val scalacheck = Seq(
    "org.scalacheck"      %% "scalacheck"      % "1.13.5",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
  )
  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.6"
}
