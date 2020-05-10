import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

//noinspection TypeAnnotation
object Dependencies {

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  private def akkaHttpModule(module: String)               = "com.typesafe.akka"             %% module            % "10.1.8"
  private def nettyModule(module: String)                  = "io.netty"                      % s"netty-$module"   % "4.1.33.Final"
  private def kamonModule(module: String)                  = "io.kamon"                      %% s"kamon-$module"  % "2.1.0"
  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.9.8"
  private def bouncyCastle(module: String)                 = "org.bouncycastle"              % s"$module-jdk15on" % "1.59"

  private def catsModule(module: String)  = Def.setting("org.typelevel" %%% s"cats-$module"  % "2.0.0")
  private def monixModule(module: String) = Def.setting("io.monix"      %%% s"monix-$module" % "3.1.0")

  private val kindProjector = compilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.6")
  private val paradise      = compilerPlugin("org.scalamacros" % "paradise"        % "2.1.1" cross CrossVersion.full)

  val akkaHttp                   = akkaHttpModule("akka-http")
  private val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  private val googleGuava        = "com.google.guava" % "guava" % "27.0.1-jre"
  private val kamonCore          = kamonModule("core")
  private val machinist          = "org.typelevel" %% "machinist" % "0.6.6"
  val logback                    = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val janino                     = "org.codehaus.janino" % "janino" % "3.0.12"
  val asyncHttpClient            = "org.asynchttpclient" % "async-http-client" % "2.7.0"

  private val catsEffect = catsModule("effect")
  private val catsCore   = catsModule("core")
  private val shapeless  = Def.setting("com.chuusai" %%% "shapeless" % "2.3.3")

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.6" % Test

  val kafka = "org.apache.kafka" %% "kafka" % "2.1.0"

  val enforcedVersions = Def.setting(
    Seq(
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      jacksonModuleScala,
      scalaTest,
      googleGuava,
      "org.slf4j" % "slf4j-api" % "1.7.25",
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      kamonCore,
      "com.typesafe" % "config" % "1.3.3",
      machinist, //.exclude("org.scala-js", "scalajs-library_2.12"), // ?
      "com.squareup.okhttp3" % "okhttp"      % "3.11.0",
      "com.squareup.okio"    % "okio"        % "1.14.0",
      "com.lihaoyi"          %% "sourcecode" % "0.1.4",
      nettyModule("handler"),
      bouncyCastle("bcpkix"),
      bouncyCastle("bcprov"),
      "org.apache.httpcomponents" % "httpcore"         % "4.4.5",
      "org.javassist"             % "javassist"        % "3.21.0-GA",
      "org.reactivestreams"       % "reactive-streams" % "1.0.2",
      "org.scala-lang"            % "scala-library"    % scalaVersion.value,
      "org.scala-lang"            % "scala-reflect"    % scalaVersion.value,
      catsEffect.value,
      catsCore.value,
      catsModule("kernel").value,
      catsModule("macros").value,
      shapeless.value,
      kafka
    )
  )

  val console = Seq("com.github.scopt" %% "scopt" % "4.0.0-RC2")

  val common = Def.setting(
    Seq(
      scalaTest
    )
  )

  val lang = Def.setting(
    Seq(
      // defined here because %%% can only be used within a task or setting macro
      // explicit dependency can likely be removed when monix 3 is released
      monixModule("eval").value
        .exclude("org.typelevel", "cats-effect_sjs0.6_2.12")
        .exclude("org.scala-js", "scalajs-library_2.12"),
      catsCore.value.exclude("org.scala-js", "scalajs-library_2.12"),
      ("org.rudogma"   %%% "supertagged" % "1.4").exclude("org.scala-js", "scalajs-library_2.12"),
      ("com.lihaoyi"   %%% "fastparse"   % "1.0.0").exclude("org.scala-js", "scalajs-library_2.12"),
      ("org.parboiled" %%% "parboiled"   % "2.1.8").exclude("org.scala-js", "scalajs-library_2.12"),
      shapeless.value.exclude("org.scala-js", "scalajs-library_2.12"),
      machinist.exclude("org.scala-js", "scalajs-library_2.12"),
      catsEffect.value.exclude("org.typelevel", "cats-core_sjs0.6_2.12"),
      ("org.typelevel" %% "cats-mtl-core" % "0.4.0").exclude("org.scalacheck", "scalacheck_2.12"),
      "ch.obermuhlner" % "big-math" % "2.1.0",
      ("org.scorexfoundation" %% "scrypto" % "2.0.4").exclude("org.whispersystems", "curve25519-java"),
      "com.wavesplatform" % "curve25519-java" % "0.6.3",
      ("org.bykn" %% "fastparse-cats-core" % "0.1.0")
        .exclude("org.scalatest", "scalatest_2.12")
        .exclude("org.scalacheck", "scalacheck_2.12")
        .exclude("org.typelevel", "cats-testkit_2.12"),
      bouncyCastle("bcpkix"),
      bouncyCastle("bcprov"),
      kindProjector,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4"),
      "com.softwaremill.sttp" %%% "core" % "1.6.4",
      "com.wavesplatform"     % "zwaves" % "0.1.0-SNAPSHOT"
    ) ++ scalapbRuntime.value ++ circe.value
  )

  lazy val it = scalaTest +: Seq(
    logback,
    "com.spotify" % "docker-client" % "8.15.1",
    jacksonModule("dataformat", "dataformat-properties"),
    asyncHttpClient,
    "org.scalacheck" %% "scalacheck" % "1.14.0"
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalacheck" %% "scalacheck" % "1.14.0",
    ("io.github.amrhassan" %% "scalacheck-cats" % "0.4.0").exclude("org.scalacheck", "scalacheck_2.12"),
    "org.mockito"   % "mockito-all"                  % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0"
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback             % Runtime,
    janino              % Runtime,
    akkaModule("slf4j") % Runtime
  )

  private[this] val levelDBJNA = {
    val levelDbVersion = "1.22.1"
    Seq(
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-core"   % levelDbVersion,
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-native" % levelDbVersion classifier "linux-x86_64",
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-native" % levelDbVersion classifier "windows-x86_64",
      "com.wavesplatform.leveldb-jna" % "leveldb-jna-native" % levelDbVersion classifier "osx"
    )
  }

  lazy val node = Def.setting(
    Seq(
      "commons-net"          % "commons-net"              % "3.6",
      "org.apache.commons"   % "commons-lang3"            % "3.9",
      "com.iheart"           %% "ficus"                   % "1.4.2",
      "net.logstash.logback" % "logstash-logback-encoder" % "4.11" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      "org.influxdb" % "influxdb-java" % "2.14",
      googleGuava,
      "com.google.code.findbugs" % "jsr305"         % "3.0.2" % Compile, // javax.annotation stubs
      "com.typesafe.play"        %% "play-json"     % "2.7.1",
      "org.ethereum"             % "leveldbjni-all" % "1.18.3",
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      kindProjector,
      paradise,
      monixModule("reactive").value,
      nettyModule("handler"),
      "io.estatico"                       %% "newtype" % "0.4.3",
      akkaModule("testkit")               % Test,
      akkaHttpModule("akka-http-testkit") % Test,
      ("org.iq80.leveldb" % "leveldb" % "0.12").exclude("com.google.guava", "guava") % Test
    ) ++ protobuf.value ++ test ++ console ++ logDeps ++ levelDBJNA
  )

  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.2.5-SC-601-SNAPSHOT" classifier "proto"

  lazy val scalapbRuntime = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf",
      "com.thesamet.scalapb" %% "scalapb-json4s"   % "0.7.0"
    )
  }

  lazy val protobuf = Def.setting {
    scalapbRuntime.value :+ protoSchemasLib % "protobuf"
  }

  lazy val grpc: Seq[ModuleID] = Seq(
    "io.grpc"              % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    protoSchemasLib        % "protobuf"
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.12.0-RC4"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }

  lazy val circeJsInterop = Def.setting {
    "io.circe" %%% "not-java-time" % "0.2.0"
  }
}
