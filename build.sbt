import com.typesafe.config.ConfigFactory
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys._
import sbt._

enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging, JDebPackaging, SystemdPlugin)

val appConf = ConfigFactory.parseFile(new File("src/main/resources/reference.conf")).resolve().getConfig("app")

name := "waves"
organization := "com.wavesplatform"
version := appConf.getString("version")
scalaVersion := "2.12.1"
crossPaths := false
publishArtifact in (Compile, packageDoc) := false
publishArtifact in (Compile, packageSrc) := false
mainClass in Compile := Some("com.wavesplatform.Application")
scalacOptions ++= Seq("-feature", "-deprecation", "-Xmax-classfile-name", "128")

//assembly settings
assemblyJarName in assembly := "waves.jar"
test in assembly := {}

libraryDependencies ++=
  Dependencies.db ++
  Dependencies.http ++
  Dependencies.akka ++
  Dependencies.serialization ++
  Dependencies.testKit ++
  Dependencies.itKit ++
  Dependencies.logging ++
  Dependencies.matcher ++
  Dependencies.p2p ++
  Seq(
    "com.iheart" %% "ficus" % "1.4.0",
    "org.scorexfoundation" %% "scrypto" % "1.2.0",
    "commons-net" % "commons-net" % "3.+"
  )

inConfig(Test)(Seq(
  logBuffered := false,
  parallelExecution := false,
  testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports")
))

concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

Defaults.itSettings
configs(IntegrationTest)
inConfig(IntegrationTest)(Seq(
  fork := true,
  parallelExecution := false,
  javaOptions ++= Seq(
    s"-Ddocker.imageId=${docker.value.id}",
    "-Dlogback.configurationFile=logback-it.xml"
  ),
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
  // JVM memory tuning for 4g ram
  "-J-Xms128m",
  "-J-Xmx4g",

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
  """.stripMargin

inConfig(Debian)(Seq(
  debianPackageDependencies += "java8-runtime-headless",
  serviceAutostart := false,
  maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm"))
))
