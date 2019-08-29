import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

//noinspection TypeAnnotation
object Dependencies {

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  private def akkaHttpModule(module: String)               = "com.typesafe.akka"             %% module            % "10.1.8"
  private def nettyModule(module: String)                  = "io.netty"                      % s"netty-$module"   % "4.1.33.Final"
  private def kamonModule(module: String, v: String)       = "io.kamon"                      %% s"kamon-$module"  % v
  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.9.8"
  private def bouncyCastle(module: String)                 = "org.bouncycastle"              % s"$module-jdk15on" % "1.59"

  private def catsModule(module: String, version: String = "1.6.0") = Def.setting("org.typelevel" %%% s"cats-$module"  % version)
  private def monixModule(module: String)                           = Def.setting("io.monix"      %%% s"monix-$module" % "3.0.0-RC4")

  private val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

  val akkaHttp                   = akkaHttpModule("akka-http")
  private val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  private val googleGuava        = "com.google.guava" % "guava" % "27.0.1-jre"
  private val kamonCore          = kamonModule("core", "1.1.5")
  private val machinist          = "org.typelevel" %% "machinist" % "0.6.6"
  private val logback            = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val janino                     = "org.codehaus.janino" % "janino" % "3.0.12"

  private val catsEffect = catsModule("effect", "1.2.0")
  private val catsCore   = catsModule("core")
  private val shapeless  = Def.setting("com.chuusai" %%% "shapeless" % "2.3.3")

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.6" % Test

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
      shapeless.value
    ))

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
      ("org.rudogma" %%% "supertagged" % "1.4").exclude("org.scala-js", "scalajs-library_2.12"),
      ("com.lihaoyi" %%% "fastparse"   % "1.0.0").exclude("org.scala-js", "scalajs-library_2.12"),
      shapeless.value.exclude("org.scala-js", "scalajs-library_2.12"),
      machinist.exclude("org.scala-js", "scalajs-library_2.12"),
      catsEffect.value.exclude("org.typelevel", "cats-core_sjs0.6_2.12"),
      ("org.typelevel" %% "cats-mtl-core" % "0.4.0").exclude("org.scalacheck", "scalacheck_2.12"),
      "ch.obermuhlner"       % "big-math" % "2.1.0",
      "org.scorexfoundation" %% "scrypto" % "2.0.4",
      ("org.bykn" %% "fastparse-cats-core" % "0.1.0")
        .exclude("org.scalatest", "scalatest_2.12")
        .exclude("org.scalacheck", "scalacheck_2.12")
        .exclude("org.typelevel", "cats-testkit_2.12"),
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      bouncyCastle("bcpkix"),
      bouncyCastle("bcprov"),
      kindProjector,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
    ) ++ protobuf.value
  )

  lazy val it = scalaTest +: Seq(
    logback,
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.15.1").classifier("shaded"),
    jacksonModule("dataformat", "dataformat-properties"),
    "org.asynchttpclient" % "async-http-client" % "2.7.0",
    "org.scalacheck"      %% "scalacheck"       % "1.14.0"
  ).map(_ % Test)

  lazy val test = scalaTest +: Seq(
    logback,
    "org.scalacheck" %% "scalacheck" % "1.14.0",
    ("io.github.amrhassan" %% "scalacheck-cats" % "0.4.0").exclude("org.scalacheck", "scalacheck_2.12"),
    "org.mockito"   % "mockito-all"                  % "1.10.19",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0"
  ).map(_ % Test)

  lazy val node = Def.setting(
    Seq(
      "commons-net"          % "commons-net" % "3.6",
      "com.iheart"           %% "ficus" % "1.4.2",
      logback                % Runtime,
      janino                 % Runtime,
      "net.logstash.logback" % "logstash-logback-encoder" % "4.11" % Runtime,
      kamonCore,
      kamonModule("system-metrics", "1.0.0"),
      kamonModule("influxdb", "1.0.2"),
      "org.influxdb" % "influxdb-java" % "2.14",
      googleGuava,
      "com.google.code.findbugs" % "jsr305"         % "3.0.2" % Compile, // javax.annotation stubs
      "com.typesafe.play"        %% "play-json"     % "2.7.1",
      "org.ethereum"             % "leveldbjni-all" % "1.18.3",
      "com.github.swagger-akka-http" %% "swagger-akka-http" % "1.1.0",
      "javax.xml.bind" % "jaxb-api" % "2.3.1", // javax.xml.bind replacement for JAXB in swagger
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      akkaModule("slf4j") % Runtime,
      kindProjector,
      monixModule("reactive").value,
      nettyModule("handler"),
      akkaModule("testkit")               % Test,
      akkaHttpModule("akka-http-testkit") % Test,
      ("org.iq80.leveldb" % "leveldb" % "0.12").exclude("com.google.guava", "guava") % Test
    ) ++ protobuf.value ++ test ++ console
  )

  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.0.0-SNAPSHOT" classifier "proto"
  lazy val protobuf = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf",
      protoSchemasLib        % "protobuf"
    )
  }

  lazy val grpc: Seq[ModuleID] = Seq(
    "io.grpc"              % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    protoSchemasLib        % "protobuf"
  )
}
