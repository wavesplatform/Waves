import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt.{Def, _}

//noinspection TypeAnnotation
object Dependencies {
  // Node protobuf schemas
  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.5.0.1-77-SNAPSHOT" classifier "protobuf-src" intransitive ()

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.6.20"

  private def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.2.10"

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.5.12"

  private def web3jModule(module: String) = "org.web3j" % module % "4.9.5"

  def monixModule(module: String): Def.Initialize[ModuleID] = Def.setting("io.monix" %%% s"monix-$module" % "3.4.1")

  val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

  val akkaHttp        = akkaHttpModule("akka-http")
  val googleGuava     = "com.google.guava"    % "guava"             % "31.1-jre"
  val kamonCore       = kamonModule("core")
  val machinist       = "org.typelevel"      %% "machinist"         % "0.6.8"
  val logback         = "ch.qos.logback"      % "logback-classic"   % "1.3.5" // 1.4.x and later is built for Java 11
  val janino          = "org.codehaus.janino" % "janino"            % "3.1.9"
  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.12.3"
  val curve25519      = "com.wavesplatform"   % "curve25519-java"   % "0.6.4"
  val nettyHandler    = "io.netty"            % "netty-handler"     % "4.1.85.Final"

  val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.10")

  val scalaTest   = "org.scalatest" %% "scalatest" % "3.2.15" % Test
  val scalaJsTest = Def.setting("com.lihaoyi" %%% "utest" % "0.8.1" % Test)

  val sttp3 = "com.softwaremill.sttp.client3" % "core_2.13" % "3.5.2" // 3.6.x and later is built for Java 11

  val bouncyCastleProvider = "org.bouncycastle" % s"bcprov-jdk15on" % "1.70"

  val console = Seq("com.github.scopt" %% "scopt" % "4.1.0")

  val langCompilerPlugins = Def.setting(
    Seq(
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      kindProjector
    )
  )

  val lang = Def.setting(
    Seq(
      // defined here because %%% can only be used within a task or setting macro
      // explicit dependency can likely be removed when monix 3 is released
      monixModule("eval").value,
      "org.typelevel" %%% "cats-core" % "2.9.0",
      "com.lihaoyi"   %%% "fastparse" % "2.3.3",
      shapeless.value,
      "org.typelevel" %%% "cats-mtl" % "1.3.0",
      "ch.obermuhlner"  % "big-math" % "2.3.2",
      curve25519,
      bouncyCastleProvider,
      "com.wavesplatform" % "zwaves"       % "0.1.0-SNAPSHOT",
      "com.wavesplatform" % "zwaves-bn256" % "0.1.5-SNAPSHOT",
      web3jModule("crypto")
    ) ++ langCompilerPlugins.value ++ scalapbRuntime.value ++ protobuf.value
  )

  lazy val it = scalaTest +: Seq(
    logback,
    "com.spotify"                      % "docker-client"                 % "8.16.0",
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.14.2",
    asyncHttpClient
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0",
    "org.scalacheck"    %% "scalacheck"      % "1.17.0",
    "org.mockito"        % "mockito-all"     % "1.10.19",
    "org.scalamock"     %% "scalamock"       % "5.2.0"
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback             % Runtime,
    janino              % Runtime,
    akkaModule("slf4j") % Runtime
  )

  private[this] val dbDeps =
    Seq(
      "org.rocksdb" % "rocksdbjni" % "8.3.2"
    )

  lazy val node = Def.setting(
    Seq(
      ("org.rudogma"       %%% "supertagged"              % "2.0-RC2").exclude("org.scala-js", "scalajs-library_2.13"),
      "commons-net"          % "commons-net"              % "3.8.0",
      "org.apache.commons"   % "commons-lang3"            % "3.12.0",
      "com.iheart"          %% "ficus"                    % "1.5.2",
      "net.logstash.logback" % "logstash-logback-encoder" % "7.3" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      kamonModule("akka-http"),
      kamonModule("executors"),
      "org.influxdb" % "influxdb-java" % "2.23",
      googleGuava,
      "com.google.code.findbugs" % "jsr305"    % "3.0.2" % Compile, // javax.annotation stubs
      "com.typesafe.play"       %% "play-json" % "2.9.4",
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      kindProjector,
      monixModule("reactive").value,
      nettyHandler,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "eu.timepit"                 %% "refined"       % "0.10.2" exclude ("org.scala-lang.modules", "scala-xml_2.13"),
      "eu.timepit"                 %% "refined-cats"  % "0.10.2" exclude ("org.scala-lang.modules", "scala-xml_2.13"),
      "com.esaulpaugh"              % "headlong"      % "9.2.0",
      web3jModule("abi"),
      akkaModule("testkit")               % Test,
      akkaHttpModule("akka-http-testkit") % Test
    ) ++ test ++ console ++ logDeps ++ dbDeps ++ protobuf.value ++ langCompilerPlugins.value
  )

  lazy val scalapbRuntime = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf"
    )
  }

  lazy val protobuf = Def.setting {
    scalapbRuntime.value :+ protoSchemasLib % "protobuf"
  }

  lazy val grpc: Seq[ModuleID] = Seq(
    "io.grpc"               % "grpc-netty"           % scalapb.compiler.Version.grpcJavaVersion,
    "io.grpc"               % "grpc-services"        % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    protoSchemasLib         % "protobuf"
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.14.3"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }

  lazy val kanela =
    Seq("io.kamon" % "kanela-agent" % "1.0.17")
}
