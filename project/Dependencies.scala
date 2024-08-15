import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.{Def, *}
import scalapb.compiler.Version.scalapbVersion

//noinspection TypeAnnotation
object Dependencies {
  // Node protobuf schemas
  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.5.2" classifier "protobuf-src" intransitive ()

  private def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.6.21"

  private def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.2.10"

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.7.3"

  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.15.3"

  private def web3jModule(module: String) = "org.web3j" % module % "4.9.8" // 4.10+ requires Java 17 https://github.com/web3j/web3j/issues/1907

  def monixModule(module: String): Def.Initialize[ModuleID] = Def.setting("io.monix" %%% s"monix-$module" % "3.4.1")

  private def grpcModule(module: String) = "io.grpc" % module % "1.62.2"

  val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full)

  val akkaHttp        = akkaHttpModule("akka-http")
  val googleGuava     = "com.google.guava"    % "guava"             % "33.2.1-jre"
  val kamonCore       = kamonModule("core")
  val machinist       = "org.typelevel"      %% "machinist"         % "0.6.8"
  val logback         = "ch.qos.logback"      % "logback-classic"   % "1.5.6"
  val janino          = "org.codehaus.janino" % "janino"            % "3.1.12"
  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.12.3"
  val curve25519      = "com.wavesplatform"   % "curve25519-java"   % "0.6.6"
  val nettyHandler    = "io.netty"            % "netty-handler"     % "4.1.100.Final"

  val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.12")

  val playJson = "com.typesafe.play" %% "play-json" % "2.10.6"

  val scalaTest   = "org.scalatest" %% "scalatest" % "3.2.19" % Test
  val scalaJsTest = Def.setting("com.lihaoyi" %%% "utest" % "0.8.3" % Test)

  val sttp3      = "com.softwaremill.sttp.client3"  % "core_2.13" % "3.9.7"
  val sttp3Monix = "com.softwaremill.sttp.client3" %% "monix"     % "3.9.7"

  val bouncyCastleProvider = "org.bouncycastle" % s"bcprov-jdk18on" % "1.78.1"

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
      "org.typelevel" %%% s"cats-core" % "2.10.0",
      "com.lihaoyi"   %%% "fastparse"  % "3.0.2",
      shapeless.value,
      "org.typelevel" %%% "cats-mtl" % "1.4.0",
      "ch.obermuhlner"  % "big-math" % "2.3.2",
      googleGuava, // BaseEncoding.base16()
      curve25519,
      bouncyCastleProvider,
      "com.wavesplatform" % "zwaves" % "0.2.1",
      web3jModule("crypto").excludeAll(ExclusionRule("org.bouncycastle", "bcprov-jdk15on")),
    ) ++ langCompilerPlugins.value ++ scalapbRuntime.value ++ protobuf.value
  )

  lazy val it = scalaTest +: Seq(
    logback,
    "com.github.jnr"                   % "jnr-unixsocket"                % "0.38.22", // To support Apple ARM
    "com.spotify"                      % "docker-client"                 % "8.16.0",
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.17.1",
    asyncHttpClient
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0",
    "org.scalacheck"    %% "scalacheck"      % "1.18.0",
    "org.mockito"        % "mockito-all"     % "1.10.19",
    "org.scalamock"     %% "scalamock"       % "6.0.0"
  ).map(_ % Test)

  lazy val qaseReportDeps = Seq(
    playJson,
    ("io.qase" % "qase-api" % "3.2.1").excludeAll(ExclusionRule(organization = "javax.ws.rs"))
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback             % Runtime,
    janino              % Runtime,
    akkaModule("slf4j") % Runtime
  )

  private val rocksdb = "org.rocksdb" % "rocksdbjni" % "9.4.0"

  lazy val node = Def.setting(
    Seq(
      rocksdb,
      ("org.rudogma"       %%% "supertagged"              % "2.0-RC2").exclude("org.scala-js", "scalajs-library_2.13"),
      "commons-net"          % "commons-net"              % "3.11.1",
      "commons-io"           % "commons-io"               % "2.16.1",
      "com.iheart"          %% "ficus"                    % "1.5.2",
      "net.logstash.logback" % "logstash-logback-encoder" % "7.4" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      kamonModule("akka-http"),
      kamonModule("executors"),
      "org.influxdb" % "influxdb-java" % "2.24",
      googleGuava,
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % Compile, // javax.annotation stubs
      playJson,
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      kindProjector,
      monixModule("reactive").value,
      nettyHandler,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "eu.timepit"                 %% "refined"       % "0.11.2" exclude ("org.scala-lang.modules", "scala-xml_2.13"),
      "com.esaulpaugh"              % "headlong"      % "11.1.1",
      "com.github.jbellis"          % "jamm"          % "0.4.0", // Weighing caches
      web3jModule("abi").excludeAll(ExclusionRule("org.bouncycastle", "bcprov-jdk15on")),
      akkaModule("testkit")               % Test,
      akkaHttpModule("akka-http-testkit") % Test
    ) ++ test ++ console ++ logDeps ++ protobuf.value ++ langCompilerPlugins.value
  )

  val gProto = "com.google.protobuf" % "protobuf-java" % "3.25.2" // grpc 1.64.0 still requires 3.25

  lazy val scalapbRuntime = Def.setting(
    Seq(
      ("com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion).exclude(gProto.organization, gProto.name),
      ("com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion % "protobuf").exclude(gProto.organization, gProto.name),
      gProto,
      gProto % "protobuf"
    )
  )

  lazy val protobuf = Def.setting {
    scalapbRuntime.value :+ protoSchemasLib % "protobuf"
  }

  lazy val grpc: Seq[ModuleID] = Seq(
    grpcModule("grpc-netty"),
    grpcModule("grpc-services"),
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapbVersion,
    protoSchemasLib         % "protobuf"
  )

  lazy val rideRunner = Def.setting(
    Seq(
      rocksdb,
      "com.github.ben-manes.caffeine" % "caffeine"                 % "3.1.8",
      "net.logstash.logback"          % "logstash-logback-encoder" % "7.4" % Runtime,
      kamonModule("caffeine"),
      kamonModule("prometheus"),
      sttp3,
      sttp3Monix,
      "org.scala-lang.modules"           %% "scala-xml"              % "2.3.0", // JUnit reports
      akkaHttpModule("akka-http-testkit") % Test,
      "com.softwaremill.diffx"           %% "diffx-core"             % "0.9.0" % Test,
      "com.softwaremill.diffx"           %% "diffx-scalatest-should" % "0.9.0" % Test,
      grpcModule("grpc-inprocess")        % Test
    ) ++ Dependencies.console ++ Dependencies.logDeps ++ Dependencies.test
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.14.8"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }

  // https://github.com/sbt/sbt-javaagent#scopes
  // dist (only sbt-native-packager), because causes using logs before needed, so System.setProperty in RideRunnerWithPreparedStateApp has no effect.
  lazy val kanela =
    Seq("io.kamon" % "kanela-agent" % "1.0.18" % "dist")
}
