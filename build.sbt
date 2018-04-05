import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys.{sourceGenerators, _}
import sbt._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

enablePlugins(JavaServerAppPackaging, JDebPackaging, SystemdPlugin, GitVersioning)

val versionSource = Def.task {
  // WARNING!!!
  // Please, update the fallback version every major and minor releases.
  // This version is used then building from sources without Git repository
  // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
  val FallbackVersion = (0, 10, 0)

  val versionFile = (sourceManaged in Compile).value / "com" / "wavesplatform" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case _ => FallbackVersion
  }
  IO.write(versionFile,
    s"""package com.wavesplatform
       |
      |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin)
  Seq(versionFile)
}
val network = SettingKey[Network]("network")
network := { Network(sys.props.get("network")) }
normalizedName := network.value.name
name := "waves"

git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")
logBuffered := false

inThisBuild(Seq(
  scalaVersion := "2.12.4",
  organization := "com.wavesplatform",
  crossPaths := false,
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ywarn-unused:-implicits",
    "-Xlint"),
))

resolvers += Resolver.bintrayRepo("ethereum", "maven")

fork in run := true
javaOptions in run ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind"
)

inTask(assembly)(Seq(
  test := {},
  assemblyJarName := s"waves-all-${version.value}.jar",
  assemblyMergeStrategy := {
    case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat
    case other => (assemblyMergeStrategy in assembly).value(other)
  }
))

inConfig(Compile)(Seq(
  mainClass := Some("com.wavesplatform.Application"),
  publishArtifact in packageDoc := false,
  publishArtifact in packageSrc := false,
  sourceGenerators += versionSource
))

inConfig(Test)(Seq(
  logBuffered := false,
  parallelExecution := false,
  testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
  testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true")
))

inConfig(Linux)(Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "Waves node",
  packageDescription := "Waves node"
))

inConfig(Universal)(Seq(
  mappings += (baseDirectory.value / s"waves-${network.value}.conf" -> "doc/waves.conf.sample"),
  javaOptions ++= Seq(
    // -J prefix is required by the bash script
    "-J-server",
    // JVM memory tuning for 2g ram
    "-J-Xms128m",
    "-J-Xmx2g",
    "-J-XX:+ExitOnOutOfMemoryError",
    // Java 9 support
    "-J-XX:+IgnoreUnrecognizedVMOptions",
    "-J--add-modules=java.xml.bind",

    // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
    "-J-XX:+UseG1GC",
    "-J-XX:+UseNUMA",
    "-J-XX:+AlwaysPreTouch",

    // probably can't use these with jstack and others tools
    "-J-XX:+PerfDisableSharedMem",
    "-J-XX:+ParallelRefProcEnabled",
    "-J-XX:+UseStringDeduplication"
  )
))

val packageSource = Def.setting {
  sourceDirectory.value / "package"
}

val upstartScript = Def.task {
  val src = packageSource.value / "upstart.conf"
  val dest = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
  val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
  IO.write(dest, result)
  dest
}

linuxPackageMappings ++= Seq(
  (upstartScript.value, s"/etc/init/${packageName.value}.conf")
).map(packageMapping(_).withConfig().withPerms("644"))

linuxScriptReplacements += "detect-loader" ->
  """is_systemd() {
    |    which systemctl >/dev/null 2>&1 && \
    |    systemctl | grep -- -\.mount >/dev/null 2>&1
    |}
    |is_upstart() {
    |    /sbin/init --version | grep upstart >/dev/null 2>&1
    |}
    |""".stripMargin

inConfig(Debian)(Seq(
  linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
  debianPackageDependencies += "java8-runtime-headless",
  serviceAutostart := false,
  maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm"))
))

commands += Command.command("packageAll") { state =>
  "clean" :: "assembly" :: "debian:packageBin" :: state
}

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .settings(
      version := "0.0.1",
      test in assembly := {},
      libraryDependencies ++=
        Dependencies.cats ++
        Dependencies.scalacheck ++
        Dependencies.scorex ++
        Dependencies.scalatest ++
        Dependencies.scalactic ++
        Dependencies.monix.value ++
        Dependencies.scodec.value ++
        Dependencies.fastparse.value,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins")
    )
    .jsSettings(
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jvmSettings(libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "0.6.22" % "provided")

lazy val langJS = lang.js
lazy val langJVM = lang.jvm

lazy val node = project.in(file("."))
  .settings(
    libraryDependencies ++=
      Dependencies.network ++
      Dependencies.db ++
      Dependencies.http ++
      Dependencies.akka ++
      Dependencies.serialization ++
      Dependencies.testKit.map(_ % "test") ++
      Dependencies.logging ++
      Dependencies.matcher ++
      Dependencies.metrics ++
      Dependencies.fp ++
      Dependencies.ficus ++
      Dependencies.scorex ++
      Dependencies.commons_net ++
      Dependencies.monix.value
  )
  .aggregate(langJVM)
  .dependsOn(langJVM)

lazy val discovery = project

lazy val it = project
  .dependsOn(node)

lazy val generator = project
  .dependsOn(it)
  .settings(libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0")

lazy val dexgenerator = project
  .dependsOn(it)
  .settings(libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0")
