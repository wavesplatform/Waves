import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.Keys._
import sbt._

object Dependencies {

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.5.20"

  private def swaggerModule(module: String) = "io.swagger.core.v3" % s"swagger-$module" % "2.0.5"

  private def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.1.7"

  private def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.33.Final"

  private def kamonModule(module: String, v: String) = "io.kamon" %% s"kamon-$module" % v

  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.9.8"

  private val CatsVersion                                               = "1.6.0"
  private def catsModule(module: String, version: String = CatsVersion) = Def.setting("org.typelevel" %%% s"cats-$module" % version)

  private def bouncyCastle(module: String) = "org.bouncycastle" % s"$module-jdk15on" % "1.59"

  private def monixModule(module: String) = Def.setting("io.monix" %%% s"monix-$module" % "3.0.0-RC1")

  private val KindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

  val ScalaTest                  = "org.scalatest" %% "scalatest" % "3.0.6" % Test
  private val AkkaHttp           = akkaHttpModule("akka-http")
  private val JacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  private val GoogleGuava        = "com.google.guava" % "guava" % "27.0.1-jre"
  private val KamonCore          = kamonModule("core", "1.1.5")
  private val Machinist          = "org.typelevel" %% "machinist" % "0.6.6"
  private val Logback            = "ch.qos.logback" % "logback-classic" % "1.2.3"

  private val CatsEffect = catsModule("effect", "1.2.0")
  private val CatsCore   = catsModule("core")
  private val Shapeless  = Def.setting("com.chuusai" %%% "shapeless" % "2.3.3")

  val EnforcedVersions = Def.setting(
    Seq(
      akkaModule("actor"),
      akkaModule("stream"),
      AkkaHttp,
      JacksonModuleScala,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      ScalaTest,
      GoogleGuava,
      "org.slf4j" % "slf4j-api" % "1.7.25",
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      KamonCore,
      "com.typesafe" % "config" % "1.3.3",
      Machinist,
      CatsEffect.value,
      CatsCore.value,
      catsModule("kernel").value,
      catsModule("macros").value,
      "com.squareup.okhttp3" % "okhttp"      % "3.11.0",
      "com.squareup.okio"    % "okio"        % "1.14.0",
      "com.lihaoyi"          %% "sourcecode" % "0.1.4",
      nettyModule("handler"),
      bouncyCastle("bcpkix"),
      bouncyCastle("bcprov"),
      "org.apache.httpcomponents" % "httpcore"         % "4.4.5",
      "org.javassist"             % "javassist"        % "3.21.0-GA",
      "org.reactivestreams"       % "reactive-streams" % "1.0.2",
      Shapeless.value
    ))

  val Lang = Def.setting(
    Seq(
      // defined here because %%% can only be used within a task or setting macro
      // explicit dependency can likely be removed when monix 3 is released
      monixModule("eval").value,
      CatsCore.value,
      "org.rudogma" %%% "supertagged" % "1.4",
      "com.lihaoyi" %%% "fastparse"   % "1.0.0",
      Shapeless.value,
      Machinist,
      CatsEffect.value,
      ("org.typelevel" %% "cats-mtl-core" % "0.4.0")
        .exclude("org.scalacheck", "scalacheck_2.12"),
      "org.scorexfoundation" %% "scrypto" % "2.0.4",
      ("org.bykn" %% "fastparse-cats-core" % "0.1.0")
        .exclude("org.scalatest", "scalatest_2.12")
        .exclude("org.scalacheck", "scalacheck_2.12")
        .exclude("org.typelevel", "cats-testkit_2.12"),
      KindProjector,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
    ))

  lazy val itTest = ScalaTest +: Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    ("com.spotify" % "docker-client" % "8.15.1").classifier("shaded"),
    jacksonModule("dataformat", "dataformat-properties"),
    "org.asynchttpclient" % "async-http-client" % "2.7.0",
    "org.scalacheck" %% "scalacheck" % "1.14.0"
  ).map(_ % Test)

  lazy val test = ScalaTest +: Seq(
    Logback,
    "org.scalacheck"      %% "scalacheck"                  % "1.14.0",
    "io.github.amrhassan" %% "scalacheck-cats"             % "0.4.0",
    "org.mockito"         % "mockito-all"                  % "1.10.19",
    "org.scalamock"       %% "scalamock-scalatest-support" % "3.6.0"
  ).map(_ % Test)

  lazy val Node = Def.setting(
    Seq(
      "commons-net"          % "commons-net" % "3.6",
      "com.iheart"           %% "ficus" % "1.4.2",
      Logback                % "runtime",
      "net.logstash.logback" % "logstash-logback-encoder" % "4.11" % "runtime",
      KamonCore,
      kamonModule("system-metrics", "1.0.0"),
      kamonModule("akka-2.5", "1.1.1"),
      kamonModule("influxdb", "1.0.2"),
      "org.influxdb" % "influxdb-java" % "2.14",
      GoogleGuava,
      "com.google.code.findbugs" % "jsr305"         % "3.0.2" % "compile", // javax.annotation stubs
      "com.typesafe.play"        %% "play-json"     % "2.7.1",
      "org.ethereum"             % "leveldbjni-all" % "1.18.3",
      // "io.swagger"                   %% "swagger-scala-module" % "1.0.4",
      "com.github.swagger-akka-http" %% "swagger-akka-http" % "1.0.0",
      jacksonModule("core", "databind"),
      JacksonModuleScala,
      AkkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      akkaModule("persistence"),
      akkaModule("slf4j"),
      KindProjector,
      monixModule("reactive").value,
      nettyModule("handler"),
      akkaHttpModule("akka-http-testkit") % "test"
    ) ++ test)

  lazy val protobuf = Def.setting {
    val version = scalapb.compiler.Version.scalapbVersion
    Seq(
      // "com.google.protobuf" % "protobuf-java" % "3.4.0",
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % version % "protobuf",
      "com.thesamet.scalapb" %% "scalapb-json4s"   % "0.7.0"
    )
  }

  lazy val grpc = Seq(
    "io.grpc"              % "grpc-netty"            % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion
  )
}
