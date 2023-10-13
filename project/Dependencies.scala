import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import sbt.{Def, *}

//noinspection TypeAnnotation
object Dependencies {
  // Node protobuf schemas
  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.4.6-SNAPSHOT" classifier "protobuf-src" intransitive ()

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.6.21"

  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.2.10"

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.6.3"

  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.15.2"

  private def catsModule(module: String, version: String = "2.6.1") = Def.setting("org.typelevel" %%% s"cats-$module" % version)

  private def web3jModule(module: String) = "org.web3j" % module % "4.9.8" // https://github.com/web3j/web3j/issues/1907

  def monixModule(module: String): Def.Initialize[ModuleID] = Def.setting("io.monix" %%% s"monix-$module" % "3.4.1")

  val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

  val akkaHttp           = akkaHttpModule("akka-http")
  val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  val googleGuava        = "com.google.guava"    % "guava"             % "32.0.1-jre"
  val kamonCore          = kamonModule("core")
  val machinist          = "org.typelevel"      %% "machinist"         % "0.6.8"
  val logback            = "ch.qos.logback"      % "logback-classic"   % "1.3.11" // 1.4.x and later is built for Java 11
  val janino             = "org.codehaus.janino" % "janino"            % "3.1.10"
  val asyncHttpClient    = "org.asynchttpclient" % "async-http-client" % "2.12.3"
  val curve25519         = "com.wavesplatform"   % "curve25519-java"   % "0.6.6"
  val nettyHandler       = "io.netty"            % "netty-handler"     % "4.1.94.Final"

  val catsCore  = catsModule("core", "2.9.0")
  val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.10")

  val playJson = "com.typesafe.play" %% "play-json" % "2.9.4"

  val scalaTest   = "org.scalatest" %% "scalatest" % "3.2.16" % Test
  val scalaJsTest = Def.setting("com.lihaoyi" %%% "utest" % "0.8.1" % Test)

  val sttp3      = "com.softwaremill.sttp.client3"  % "core_2.13" % "3.5.2" // 3.6.x and later is built for Java 11
  val sttp3Monix = "com.softwaremill.sttp.client3" %% "monix"     % "3.5.2"

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
      catsCore.value,
      "com.lihaoyi" %%% "fastparse" % "2.3.3",
      shapeless.value,
      "org.typelevel" %%% "cats-mtl" % "1.3.1",
      "ch.obermuhlner"  % "big-math" % "2.3.2",
      googleGuava,
      curve25519,
      bouncyCastleProvider,
      "com.wavesplatform" % "zwaves" % "0.1.7-23-SNAPSHOT",
      web3jModule("crypto")
    ) ++ langCompilerPlugins.value ++ scalapbRuntime.value ++ protobuf.value
  )

  lazy val it = scalaTest +: Seq(
    logback,
    "com.spotify" % "docker-client" % "8.16.0",
    jacksonModule("dataformat", "dataformat-properties"),
    asyncHttpClient
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0",
    "org.scalacheck"    %% "scalacheck"      % "1.17.0",
    "org.mockito"        % "mockito-all"     % "1.10.19",
    "org.scalamock"     %% "scalamock"       % "5.2.0"
  ).map(_ % Test)

  lazy val qaseReportDeps = Seq(
    playJson,
    ("io.qase" % "qase-api" % "3.0.5").excludeAll(ExclusionRule(organization = "javax.ws.rs"))
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback             % Runtime,
    janino              % Runtime,
    akkaModule("slf4j") % Runtime
  )

  def leveldbJava(module: String = "") = "org.iq80.leveldb" % s"leveldb${if (module.nonEmpty) "-" else ""}$module" % "0.12"

  private[this] val levelDBJNA = {
    val levelDbVersion = "1.23.1"
    Seq(
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-core"   % levelDbVersion,
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-native" % levelDbVersion,
      leveldbJava("api")
    )
  }

  lazy val node = Def.setting(
    Seq(
      ("org.rudogma"       %%% "supertagged"              % "2.0-RC2").exclude("org.scala-js", "scalajs-library_2.13"),
      "commons-net"          % "commons-net"              % "3.9.0",
      "org.apache.commons"   % "commons-lang3"            % "3.12.0",
      "com.iheart"          %% "ficus"                    % "1.5.2",
      "net.logstash.logback" % "logstash-logback-encoder" % "7.4" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      kamonModule("akka-http"),
      kamonModule("executors"),
      "org.influxdb" % "influxdb-java" % "2.23",
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
      "eu.timepit"                 %% "refined"       % "0.11.0",
      "com.esaulpaugh"              % "headlong"      % "9.3.0",
      web3jModule("abi"),
      akkaModule("testkit")                              % Test,
      akkaHttpModule("akka-http-testkit")                % Test,
      leveldbJava().exclude("com.google.guava", "guava") % Test
    ) ++ test ++ console ++ logDeps ++ levelDBJNA ++ protobuf.value ++ langCompilerPlugins.value
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

  lazy val rideRunner = Def.setting(
    Seq(
      "org.rocksdb"           % "rocksdbjni"     % "8.3.2",
      "com.thesamet.scalapb" %% "scalapb-json4s" % "0.11.1",
      // https://github.com/netty/netty/wiki/Native-transports
      // "io.netty"                      % "netty-transport-native-epoll"  % "4.1.79.Final" classifier "linux-x86_64",
      "com.github.ben-manes.caffeine" % "caffeine"                 % "3.1.2",
      "net.logstash.logback"          % "logstash-logback-encoder" % "7.2" % Runtime,
      "org.ehcache"                   % "sizeof"                   % "0.4.3", // Weighing caches
      kamonModule("caffeine"),
      kamonModule("prometheus"),
      Dependencies.sttp3,
      Dependencies.sttp3Monix,
      "org.scala-lang.modules"                                       %% "scala-xml"              % "2.2.0", // JUnit reports
      Dependencies.leveldbJava().exclude("com.google.guava", "guava") % Test,
      Dependencies.akkaHttpModule("akka-http-testkit")                % Test,
      "com.softwaremill.diffx"                                       %% "diffx-core"             % "0.8.3" % Test,
      "com.softwaremill.diffx"                                       %% "diffx-scalatest-should" % "0.8.3" % Test
    ) ++ Dependencies.console ++ Dependencies.logDeps ++ Dependencies.test
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.14.5"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }

  // https://github.com/sbt/sbt-javaagent#scopes
  // dist (only sbt-native-packager), because causes using logs before needed, so System.setProperty in RideRunnerWithPreparedStateApp has no effect.
  lazy val kanela =
    Seq("io.kamon" % "kanela-agent" % "1.0.17" % "dist")
}
