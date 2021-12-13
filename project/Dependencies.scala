import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.{Def, _}
import sbt.Keys._

//noinspection TypeAnnotation
object Dependencies {
  // Node protobuf schemas
  private[this] val protoSchemasLib =
    "com.wavesplatform" % "protobuf-schemas" % "1.4.1-SNAPSHOT" classifier "protobuf-src" intransitive ()

  def akkaModule(module: String): ModuleID = "com.typesafe.akka" %% s"akka-$module" % "2.6.17"

  private def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.2.7"

  private def kamonModule(module: String) = "io.kamon" %% s"kamon-$module" % "2.3.1"

  private def jacksonModule(group: String, module: String) = s"com.fasterxml.jackson.$group" % s"jackson-$module" % "2.13.0"

  private def catsModule(module: String, version: String = "2.6.1") = Def.setting("org.typelevel" %%% s"cats-$module" % version)

  private def web3jModule(module: String) = "org.web3j" % module % "4.8.7"

  def monixModule(module: String): Def.Initialize[ModuleID] = Def.setting("io.monix" %%% s"monix-$module" % "3.4.0")

  val kindProjector = compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

  val akkaHttp           = akkaHttpModule("akka-http")
  val jacksonModuleScala = jacksonModule("module", "module-scala").withCrossVersion(CrossVersion.Binary())
  val googleGuava        = "com.google.guava" % "guava" % "31.0.1-jre"
  val kamonCore          = kamonModule("core")
  val machinist          = "org.typelevel" %% "machinist" % "0.6.8"
  val logback            = "ch.qos.logback" % "logback-classic" % "1.2.6"
  val janino             = "org.codehaus.janino" % "janino" % "3.1.6"
  val asyncHttpClient    = "org.asynchttpclient" % "async-http-client" % "2.12.3"
  val curve25519         = "com.wavesplatform" % "curve25519-java" % "0.6.4"
  val nettyHandler       = "io.netty" % "netty-handler" % "4.1.69.Final"

  val catsEffect = catsModule("effect", "2.1.3")
  val catsCore   = catsModule("core")
  val shapeless  = Def.setting("com.chuusai" %%% "shapeless" % "2.3.7")

  val scalaTest = "org.scalatest" %% "scalatest" % "3.2.10" % Test

  val sttp3 = "com.softwaremill.sttp.client3" % "core_2.13" % "3.3.14"

  // v1.67 introduced unnecessary conversions which slowed down hash computation by a factor of 3-4:
  // https://github.com/bcgit/bc-java/blob/r1rv67/core/src/main/java/org/bouncycastle/crypto/digests/KeccakDigest.java#L318
  // Before upping the version, make sure conversions are no longer there.
  val bouncyCastleProvider = "org.bouncycastle" % s"bcprov-jdk15on" % "1.66"

  val enforcedVersions = Def.setting(
    Seq(
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      jacksonModuleScala,
      scalaTest,
      googleGuava,
      "org.slf4j" % "slf4j-api" % "1.7.32",
      jacksonModule("core", "core"),
      jacksonModule("core", "annotations"),
      jacksonModule("core", "databind"),
      jacksonModule("dataformat", "dataformat-yaml"),
      jacksonModule("dataformat", "dataformat-properties"),
      jacksonModule("jaxrs", "jaxrs-base"),
      jacksonModule("jaxrs", "jaxrs-json-provider"),
      kamonCore,
      "com.typesafe" % "config" % "1.4.1",
      machinist,
      "com.squareup.okhttp3" % "okhttp"      % "4.9.1",
      "com.squareup.okio"    % "okio"        % "2.10.0",
      "com.lihaoyi"          %% "sourcecode" % "0.2.7",
      nettyHandler,
      bouncyCastleProvider,
      "org.apache.httpcomponents" % "httpcore"         % "4.4.14",
      "org.javassist"             % "javassist"        % "3.21.0-GA",
      "org.reactivestreams"       % "reactive-streams" % "1.0.3",
      "org.scala-lang"            % "scala-library"    % scalaVersion.value,
      "org.scala-lang"            % "scala-reflect"    % scalaVersion.value,
      catsEffect.value,
      catsCore.value,
      catsModule("kernel").value,
      catsModule("macros", "2.1.1").value,
      shapeless.value
    )
  )

  val console = Seq("com.github.scopt" %% "scopt" % "4.0.1")

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
      catsCore.value.exclude("org.scala-js", "scalajs-library_2.13"),
      ("com.lihaoyi"   %%% "fastparse" % "2.3.3").exclude("org.scala-js", "scalajs-library_2.13"),
      ("org.parboiled" %%% "parboiled" % "2.3.0").exclude("org.scala-js", "scalajs-library_2.13"),
      shapeless.value.exclude("org.scala-js", "scalajs-library_2.13"),
      ("org.typelevel" %% "cats-mtl-core" % "0.7.1").exclude("org.scalacheck", "scalacheck_2.13"),
      "ch.obermuhlner" % "big-math" % "2.3.0",
      curve25519,
      bouncyCastleProvider,
      "com.wavesplatform" % "zwaves"       % "0.1.0-SNAPSHOT",
      "com.wavesplatform" % "zwaves-bn256" % "0.1.5-SNAPSHOT",
      web3jModule("crypto"),
      web3jModule("abi"),
      web3jModule("rlp"),
      "com.esaulpaugh" % "headlong" % "5.4.0"
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
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0",
    "org.scalacheck"    %% "scalacheck"      % "1.15.4",
    "org.mockito"       % "mockito-all"      % "1.10.19",
    "org.scalamock"     %% "scalamock"       % "5.1.0"
  ).map(_ % Test)

  lazy val logDeps = Seq(
    logback             % Runtime,
    janino              % Runtime,
    akkaModule("slf4j") % Runtime
  )

  private def leveldbJava(module: String = "") = "org.iq80.leveldb" % s"leveldb${if (module.nonEmpty) "-" else ""}$module" % "0.12"

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
      ("org.rudogma" %%% "supertagged" % "2.0-RC2").exclude("org.scala-js", "scalajs-library_2.13"),
      "commons-net"          % "commons-net"              % "3.8.0",
      "org.apache.commons"   % "commons-lang3"            % "3.12.0",
      "com.iheart"           %% "ficus"                   % "1.5.1",
      "net.logstash.logback" % "logstash-logback-encoder" % "6.6" % Runtime,
      kamonCore,
      kamonModule("system-metrics"),
      kamonModule("influxdb"),
      "org.influxdb" % "influxdb-java" % "2.22",
      googleGuava,
      "com.google.code.findbugs" % "jsr305"     % "3.0.2" % Compile, // javax.annotation stubs
      "com.typesafe.play"        %% "play-json" % "2.9.2",
      akkaModule("actor"),
      akkaModule("stream"),
      akkaHttp,
      "org.bitlet" % "weupnp" % "0.1.4",
      kindProjector,
      monixModule("reactive").value,
      nettyHandler,
      "com.typesafe.scala-logging"                       %% "scala-logging" % "3.9.4",
      akkaModule("testkit")                              % Test,
      akkaHttpModule("akka-http-testkit")                % Test,
      leveldbJava().exclude("com.google.guava", "guava") % Test
    ) ++ test ++ console ++ logDeps ++ levelDBJNA ++ protobuf.value
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
    "io.grpc"              % "grpc-netty" % scalapb.compiler.Version.grpcJavaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
    protoSchemasLib        % "protobuf"
  )

  lazy val circe = Def.setting {
    val circeVersion = "0.14.1"
    Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  }
}
