import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  def swaggerModule(module: String) = ("io.swagger.core.v3" % s"swagger-$module" % "2.0.5").exclude("com.google.guava", "guava")

  def akkaHttpModule(module: String = "") = "com.typesafe.akka" %% s"akka-http${if (module.isEmpty) "" else s"-$module"}" % "10.1.7"

  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.24.Final"

  def kamonModule(module: String, v: String) = "io.kamon" %% s"kamon-$module" % v
  
  val AkkaActor = akkaModule("actor")
  val AkkaStream = akkaModule("stream")
  val AkkaHTTP = akkaHttpModule()

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
    akkaHttpModule("testkit")
  )

  lazy val itKit = scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.11.3").classifier("shaded").exclude("com.google.guava", "guava"),
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.6",
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val serialization = Seq(
    "com.google.guava"         % "guava"      % "21.0",
    "com.google.code.findbugs" % "jsr305"     % "3.0.2" % "compile", // to support guava
    "com.typesafe.play"        %% "play-json" % "2.6.10"
  )

  lazy val akka = Seq(AkkaActor, akkaModule("slf4j"))

  lazy val db = Seq(
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val logging = Seq(
    "ch.qos.logback"       % "logback-classic"          % "1.2.3",
    "org.slf4j"            % "slf4j-api"                % "1.7.25",
    "org.slf4j"            % "jul-to-slf4j"             % "1.7.25",
    "net.logstash.logback" % "logstash-logback-encoder" % "4.11"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs2").map(swaggerModule) ++ Seq(
    "io.swagger"                   %% "swagger-scala-module" % "1.0.4",
    "com.github.swagger-akka-http" %% "swagger-akka-http"    % "1.0.0",
    "com.fasterxml.jackson.core"   % "jackson-databind"      % "2.9.6",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.6",
    AkkaHTTP
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    akkaModule("persistence-tck") % "test",
    "com.github.dnvriend"         %% "akka-persistence-inmemory" % "2.5.15.1" % "test",
    "com.typesafe.akka"           %% "akka-stream-kafka" % "1.0-RC2",
    "org.ethereum"                % "leveldbjni-all" % "1.18.3"
  )

  lazy val metrics = Seq(
    kamonModule("core", "1.1.3"),
    kamonModule("system-metrics", "1.0.0").exclude("io.kamon", "kamon-core_2.12"),
    kamonModule("akka-2.5", "1.1.3").exclude("io.kamon", "kamon-core_2.12"),
    kamonModule("influxdb", "1.0.2"),
    "org.influxdb" % "influxdb-java" % "2.11"
  ).map(_.exclude("org.asynchttpclient", "async-http-client"))

  lazy val fp = Seq(
    "org.typelevel"       %% "cats-core"       % "1.1.0",
    "org.typelevel"       %% "cats-mtl-core"   % "0.3.0",
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
  lazy val scalatest   = Seq("org.scalatest" %% "scalatest"  % "3.0.6")
  lazy val scalactic   = Seq("org.scalactic" %% "scalactic"  % "3.0.5")
  lazy val cats        = Seq("org.typelevel" %% "cats-core"  % "1.1.0")
  lazy val scalacheck = Seq(
    "org.scalacheck"      %% "scalacheck"      % "1.14.0",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
  )
  lazy val kindProjector = "org.spire-math" %% "kind-projector"     % "0.9.6"
  lazy val betterFor     = "com.olegpy"     %% "better-monadic-for" % "0.3.0-M4"
  
  lazy val protobuf = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      // "com.google.protobuf" % "protobuf-java" % "3.4.0",
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf",
      "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.0"
    )
  }
  
  lazy val grpc = Seq(
    "io.grpc" % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
  )
}
