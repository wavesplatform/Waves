import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

//noinspection TypeAnnotation
object Dependencies {

  private def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.6.6"

  private def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.1.12"

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.1.1"

  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.11.0"

  private def bouncyCastle(module: String, suffix: String = "") = "org.bouncycastle" % s"$module-jdk15on" % s"1.65$suffix"

  private def catsModule(module: String, version: String = "2.1.0") = Def.setting("org.typelevel" %%% s"cats-$module" % version)

  private def monixModule(module: String) = Def.setting("io.monix" %%% s"monix-$module" % "3.2.2")

  private val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)

  private val akkaHttp           = akkaHttpModule("akka-http")
  private val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  private val googleGuava        = "com.google.guava" % "guava" % "29.0-jre"
  private val kamonCore          = kamonModule("core")
  private val machinist          = "org.typelevel" %% "machinist" % "0.6.8"
  val logback                    = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val janino                     = "org.codehaus.janino" % "janino" % "3.1.2"
  val asyncHttpClient            = "org.asynchttpclient" % "async-http-client" % "2.12.1"

  private val nettyHandler = "io.netty" % "netty-handler" % "4.1.50.Final"

  private val catsEffect = catsModule("effect", "2.1.3")
  private val catsCore   = catsModule("core", "2.1.1")
  private val shapeless  = Def.setting("com.chuusai" %%% "shapeless" % "2.3.3")

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"

  val kafka = "org.apache.kafka" %% "kafka" % "2.5.0"

  private val bcpkix = bouncyCastle("bcpkix")
  private val bcprov = bouncyCastle("bcprov", ".01")

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
      machinist,
      "com.squareup.okhttp3" % "okhttp"      % "3.11.0",
      "com.squareup.okio"    % "okio"        % "1.14.0",
      "com.lihaoyi"          %% "sourcecode" % "0.2.1",
      nettyHandler,
      bcpkix,
      bcprov,
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

  val lang = Def.setting(
    Seq(
      // defined here because %%% can only be used within a task or setting macro
      // explicit dependency can likely be removed when monix 3 is released
      monixModule("eval").value,
      catsCore.value.exclude("org.scala-js", "scalajs-library_2.13"),
      ("org.rudogma"   %%% "supertagged" % "1.5").exclude("org.scala-js", "scalajs-library_2.13"),
      ("com.lihaoyi"   %%% "fastparse"   % "2.3.0").exclude("org.scala-js", "scalajs-library_2.13"),
      ("org.parboiled" %%% "parboiled"   % "2.1.8").exclude("org.scala-js", "scalajs-library_2.13"),
      shapeless.value.exclude("org.scala-js", "scalajs-library_2.13"),
      machinist.exclude("org.scala-js", "scalajs-library_2.13"),
      catsEffect.value,
      ("org.typelevel" %% "cats-mtl-core" % "0.7.1").exclude("org.scalacheck", "scalacheck_2.13"),
      "ch.obermuhlner" % "big-math" % "2.3.0",
      ("org.scorexfoundation" %% "scrypto" % "2.1.8").exclude("org.whispersystems", "curve25519-java"),
      "com.wavesplatform" % "curve25519-java" % "0.6.3",
      bcpkix,
      bcprov,
      kindProjector,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      "com.softwaremill.sttp" %%% "core" % "1.7.2",
      "com.wavesplatform"     % "zwaves" % "0.1.0-SNAPSHOT",
      "org.web3j"             % "crypto" % "5.0.0"
    ) ++ scalapbRuntime.value ++ circe.value
  )

  lazy val it = Seq(
    scalaTest,
    "com.spotify" % "docker-client" % "8.16.0",
    jacksonModule("dataformat", "dataformat-properties"),
    asyncHttpClient
  ).map(_ % Test) ++ logDeps

  lazy val test = Seq(
    scalaTest,
    "org.scalacheck" %% "scalacheck" % "1.14.3",
    "org.mockito"    % "mockito-all" % "1.10.19",
    "org.scalamock"  %% "scalamock"  % "4.4.0"
  )

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
      "org.apache.commons"   % "commons-lang3"            % "3.10",
      "com.iheart"           %% "ficus"                   % "1.4.7",
      "net.logstash.logback" % "logstash-logback-encoder" % "4.11" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      "org.influxdb" % "influxdb-java" % "2.19",
      googleGuava,
      "com.google.code.findbugs" % "jsr305"         % "3.0.2" % Compile, // javax.annotation stubs
      "com.typesafe.play"        %% "play-json"     % "2.9.0",
      "org.ethereum"             % "leveldbjni-all" % "1.18.3",
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      kindProjector,
      monixModule("reactive").value,
      nettyHandler
    ) ++ protobuf.value ++ console ++ logDeps ++ levelDBJNA
  )

  lazy val nodeTestkit = Def.setting(
    Seq(
      akkaModule("testkit"),
      akkaHttpModule("akka-http-testkit"),
      ("org.iq80.leveldb" % "leveldb" % "0.12").exclude("com.google.guava", "guava")
    ) ++ test ++ logDeps
  )

  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.2.6-SNAPSHOT" classifier "proto"

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
    "io.grpc"              % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    protoSchemasLib        % "protobuf"
  )

  lazy val circe = Def.setting {
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % "0.13.0")
  }

  lazy val circeJsInterop = Def.setting {
    "io.circe" %%% "not-java-time" % "0.2.0"
  }
}
