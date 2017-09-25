import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys._
import sbt._

enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging, JDebPackaging, SystemdPlugin, GitVersioning)

name := "waves"
organization := "com.wavesplatform"
git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")
scalaVersion in ThisBuild := "2.12.3"
crossPaths := false
publishArtifact in (Compile, packageDoc) := false
publishArtifact in (Compile, packageSrc) := false
mainClass in Compile := Some("com.wavesplatform.Application")
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-unused:-implicits",
  "-Xlint")
logBuffered := false

//assembly settings
assemblyJarName in assembly := s"waves-all-${version.value}.jar"
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat
  case other => (assemblyMergeStrategy in assembly).value(other)
}
test in assembly := {}

libraryDependencies ++=
  Dependencies.network ++
  Dependencies.db ++
  Dependencies.http ++
  Dependencies.akka ++
  Dependencies.serialization ++
  Dependencies.testKit ++
  Dependencies.itKit ++
  Dependencies.logging ++
  Dependencies.matcher ++
  Dependencies.metrics ++
  Dependencies.fp ++
  Seq(
    "com.iheart" %% "ficus" % "1.4.2",
    ("org.scorexfoundation" %% "scrypto" % "1.2.2")
      .exclude("org.slf4j", "slf4j-api"),
    "commons-net" % "commons-net" % "3.+",
    "io.monix" %% "monix" % "2.3.0"
  )

sourceGenerators in Compile += Def.task {
  val versionFile = (sourceManaged in Compile).value / "com" / "wavesplatform" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val versionExtractor(major, minor, bugfix) = version.value
  IO.write(versionFile,
    s"""package com.wavesplatform
      |
      |object Version {
      |  val VersionString = "${version.value}"
      |  val VersionTuple = ($major, $minor, $bugfix)
      |}
      |""".stripMargin)
  Seq(versionFile)
}

inConfig(Test)(Seq(
  logBuffered := false,
  parallelExecution := false,
  testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports")
))

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

Defaults.itSettings
configs(IntegrationTest)
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
  testOptions += Tests.Filter(_.endsWith("Suite"))
))

dockerfile in docker := {
  val configTemplate = (resourceDirectory in IntegrationTest).value / "template.conf"
  val startWaves = (sourceDirectory in IntegrationTest).value / "container" / "start-waves.sh"

  new Dockerfile {
    from("anapsix/alpine-java:8_server-jre")
    add(assembly.value, "/opt/waves/waves.jar")
    add(Seq(configTemplate, startWaves), "/opt/waves/")
    run("chmod", "+x", "/opt/waves/start-waves.sh")
    entryPoint("/opt/waves/start-waves.sh")
  }
}

// packaging settings
val upstartScript = TaskKey[File]("upstartScript")
val packageSource = SettingKey[File]("packageSource")
val network = SettingKey[Network]("network")

commands += Command.command("packageAll") { state =>
  "clean" ::
  "assembly" ::
  "debian:packageBin" ::
  state
}

inConfig(Linux)(Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "Waves node",
  packageDescription := "Waves node"
))

network := Network(sys.props.get("network"))
normalizedName := network.value.name

javaOptions in Universal ++= Seq(
  // -J prefix is required by the bash script
  "-J-server",
  // JVM memory tuning for 1g ram
  "-J-Xms128m",
  "-J-Xmx1g",
  "-J-XX:+ExitOnOutOfMemoryError",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-J-XX:+UseG1GC",
  "-J-XX:+UseNUMA",
  "-J-XX:+AlwaysPreTouch",

  // probably can't use these with jstack and others tools
  "-J-XX:+PerfDisableSharedMem",
  "-J-XX:+ParallelRefProcEnabled",
  "-J-XX:+UseStringDeduplication")

mappings in Universal += (baseDirectory.value / s"waves-${network.value}.conf" -> "doc/waves.conf.sample")
packageSource := sourceDirectory.value / "package"
upstartScript := {
  val src = packageSource.value / "upstart.conf"
  val dest = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
  val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
  IO.write(dest, result)
  dest
}
linuxPackageMappings ++= Seq(
  packageMapping((upstartScript.value, s"/usr/share/${packageName.value}/conf/upstart.conf"))
).map(_.withConfig().withPerms("644").withUser(packageName.value).withGroup(packageName.value))

linuxStartScriptTemplate in Debian := (packageSource.value / "systemd.service").toURI.toURL
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
  debianPackageDependencies += "java8-runtime-headless",
  serviceAutostart := false,
  maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm"))
))

lazy val node = project.in(file("."))
lazy val generator = project.in(file("generator"))
  .dependsOn(node % "compile->it")
  .settings(
    libraryDependencies ++=
      Dependencies.fp ++
      Seq(
        "com.github.scopt" %% "scopt" % "3.6.0"
      )
  )
