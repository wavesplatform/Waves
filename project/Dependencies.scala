import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.Keys.scalaVersion
import sbt.{Def, *}
import scalapb.compiler.Version.scalapbVersion

object Dependencies {
  private def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.2.15.Final"

  val gProtoVersion = "4.35.0"
  val gProto        = "com.google.protobuf" % "protobuf-java" % Dependencies.gProtoVersion
  val overrides = Def.setting(
    Seq(
      "org.scala-lang"           %% "scala3-library" % scalaVersion.value,
      "com.google.code.gson"      % "gson"           % "2.13.2",
      "com.squareup.okio"         % "okio-jvm"       % "3.17.0",
      "org.apache.httpcomponents" % "httpclient"     % "4.5.14",
      "org.slf4j"                 % "slf4j-api"      % "2.0.17",
      "org.msgpack"               % "msgpack-core"   % "0.9.11",
      nettyModule("codec-http2"),
      nettyModule("codec-http"),
      nettyModule("handler-proxy"),
      nettyModule("codec-socks"),
      nettyModule("transport-native-unix-common"),
      nettyModule("resolver-dns"),
      jacksonModule("core", "core"),
      jacksonModule("core", "databind"),
      jacksonModule("datatype", "datatype-jdk8"),
      jacksonModule("datatype", "datatype-jsr310"),
      gProto
    )
  )

  // Node protobuf schemas
  lazy val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.6.0" classifier "protobuf-src" intransitive ()

  private def pekkoModule(module: String) = "org.apache.pekko" %% s"pekko-$module" % "1.6.0"

  private def pekkoHttpModule(module: String, version: String = "1.3.0") = "org.apache.pekko" %% module % version

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.8.1"

  private def jacksonModule(group: String, module: String, version: String = "2.21.1") = s"com.fasterxml.jackson.$group" % s"jackson-$module" % version

  private def web3jModule(module: String) = "org.web3j" % module % "4.13.0" // 4.14+ requires Java 21 https://github.com/LFDT-web3j/web3j/releases/tag/v4.14.0

  def monixModule(module: String): Def.Initialize[ModuleID] = Def.setting("io.monix" %%% s"monix-$module" % "3.4.1")

  private def grpcModule(module: String) = "io.grpc" % module % "1.81.0"

  val pekkoHttp       = pekkoHttpModule("pekko-http")
  val googleGuava     = "com.google.guava"    % "guava"             % "33.6.0-jre"
  val kamonCore       = kamonModule("core")
  val machinist       = "org.typelevel"      %% "machinist"         % "0.6.8"
  val logback         = "ch.qos.logback"      % "logback-classic"   % "1.5.34"
  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "3.0.10"
  val curve25519      = "com.wavesplatform"   % "curve25519-java"   % "0.6.6"
  val nettyHandler    = nettyModule("handler")

  val playJson = "org.playframework" %% "play-json" % "3.0.6"

  val scalaTest   = "org.scalatest" %% "scalatest" % "3.2.20" % Test
  val scalaJsTest = Def.setting("com.lihaoyi" %%% "utest" % "0.9.5" % Test)

  private def sttp3Module(module: String) = "com.softwaremill.sttp.client3" %% module % "3.11.0"

  val sttp3      = sttp3Module("core")
  val sttp3Monix = sttp3Module("monix")

  val console = Seq("com.github.scopt" %% "scopt" % "4.1.0")

  def amazonCorretto(c: String): ModuleID = "software.amazon.cryptools" % "AmazonCorrettoCryptoProvider" % "2.5.0" classifier c

  val cryptoProviders = Seq(
    // Windows x86_64, Windows x86, macOS x86_64, linux x86_64
    "org.conscrypt" % "conscrypt-openjdk-uber" % "2.5.2",
    // macOS aarch64
    amazonCorretto("osx-aarch_64"),
    // fallback Java
    "org.bouncycastle" % "bcprov-jdk18on" % "1.84"
  )

  val lang = Def.setting(
    Seq(
      // defined here because %%% can only be used within a task or setting macro
      // explicit dependency can likely be removed when monix 3 is released
      monixModule("eval").value,
      "org.typelevel" %%% s"cats-core" % "2.13.0",
      "com.lihaoyi"   %%% "fastparse"  % "3.1.1",
      "org.typelevel" %%% "cats-mtl"   % "1.6.0",
      "ch.obermuhlner"  % "big-math"   % "2.3.2",
      googleGuava, // BaseEncoding.base16()
      curve25519,
      "com.wavesplatform" % "zwaves" % "0.2.1",
      web3jModule("crypto").excludeAll(ExclusionRule("org.bouncycastle", "bcprov-jdk15on")),
      protoSchemasLib % "protobuf"
    ) ++ cryptoProviders
  )

  lazy val scalapbRuntimeJS = Def.setting(
    Seq(
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion % "protobuf"
    )
  )

  lazy val it = scalaTest +: Seq(
    logback,
    "com.github.jnr" % "jnr-unixsocket" % "0.38.25", // To support Apple ARM
    "com.spotify"    % "docker-client"  % "8.16.0",
    jacksonModule("dataformat", "dataformat-properties", "2.22.0"),
    asyncHttpClient
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0",
    "org.scalacheck"    %% "scalacheck"      % "1.19.0",
    "org.mockito"        % "mockito-all"     % "1.10.19",
    "org.scalamock"     %% "scalamock"       % "6.2.0"
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback              % Runtime,
    pekkoModule("slf4j") % Runtime
  )

  // Check https://github.com/facebook/rocksdb/issues/13893 before bumping
  private val rocksdb = "org.rocksdb" % "rocksdbjni" % "10.2.1"

  val scalaLogging: ModuleID = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"
  lazy val node = Def.setting(
    Seq(
      rocksdb,
      "commons-net"            % "commons-net"               % "3.13.0",
      "commons-io"             % "commons-io"                % "2.21.0",
      "com.github.pureconfig" %% "pureconfig-core"           % "0.17.10",
      "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.10",
      "net.logstash.logback"   % "logstash-logback-encoder"  % "9.0" % Runtime,
      kamonCore,
      kamonModule("pekko-http"),
      kamonModule("executors"),
      "org.influxdb" % "influxdb-java" % "2.25",
      googleGuava,
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % Compile, // javax.annotation stubs
      playJson,
      pekkoModule("actor"),
      pekkoModule("stream"),
      pekkoHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      monixModule("reactive").value,
      nettyHandler,
      scalaLogging,
      "eu.timepit"        %% "refined"  % "0.11.3" exclude ("org.scala-lang.modules", "scala-xml_2.13"),
      "com.esaulpaugh"     % "headlong" % "13.3.1",
      "com.github.jbellis" % "jamm"     % "0.4.0", // Weighing caches
      web3jModule("abi").excludeAll(ExclusionRule("org.bouncycastle", "bcprov-jdk15on")),
      "com.wavesplatform"         % "blst-java"                    % "0.3.15-1",
      amazonCorretto("linux-x86_64") % Optional,
      amazonCorretto("linux-aarch_64") % Optional
    ) ++ console ++ logDeps ++ protobuf.value
  )

  lazy val nodeTests = Seq(
    pekkoModule("testkit"),
    pekkoHttpModule("pekko-http-testkit")
  ) ++ test ++ logDeps

  lazy val scalapbRuntime = Def.setting(
    Seq(
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
      "com.github.ben-manes.caffeine" % "caffeine"                 % "3.2.3",
      "net.logstash.logback"          % "logstash-logback-encoder" % "9.0" % Runtime,
      kamonModule("caffeine"),
      kamonModule("prometheus"),
      sttp3,
      sttp3Monix,
      "org.scala-lang.modules"             %% "scala-xml"              % "2.4.0", // JUnit reports
      pekkoHttpModule("pekko-http-testkit") % Test,
      "com.softwaremill.diffx"             %% "diffx-core"             % "0.9.0" % Test,
      "com.softwaremill.diffx"             %% "diffx-scalatest-should" % "0.9.0" % Test,
      grpcModule("grpc-inprocess")          % Test
    ) ++ Dependencies.console ++ Dependencies.logDeps ++ Dependencies.test
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.14.15"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }

  // https://github.com/sbt/sbt-javaagent#scopes
  // dist (only sbt-native-packager), because causes using logs before needed, so System.setProperty in RideRunnerWithPreparedStateApp has no effect.
  lazy val kanela =
    Seq("io.kamon" % "kanela-agent" % "2.0.0" % "dist")
}
