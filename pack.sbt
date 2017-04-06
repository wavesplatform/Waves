import com.typesafe.sbt.SbtNativePackager.autoImport._
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys._

val network = Option(System.getProperty("network")).getOrElse("mainnet")

val commonSettings: Seq[Setting[_]] = Seq(
  javaOptions in Universal ++= Seq(
    "-J-server",
    // JVM memory tuning for 1g ram
    "-J-Xms128m",
    "-J-Xmx1024m",

    // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
    "-J-XX:+UseG1GC",
    "-J-XX:+UseNUMA",
    "-J-XX:+AlwaysPreTouch",

    // may be can't use with jstack and others tools
    "-J-XX:+PerfDisableSharedMem",
    "-J-XX:+ParallelRefProcEnabled",
    "-J-XX:+UseStringDeduplication"),
  mainClass in Compile := Some("com.wavesplatform.Application")
)


val upstartScript = TaskKey[File]("upstartScript")
val packageSource = SettingKey[File]("packageSource")

val debianSettings = Seq(
  maintainer in Linux := "wavesplatform.com",
  packageName := {
    network match {
      case "mainnet" => "waves"
      case "testnet" => "waves-testnet"
      case "devnet" => "waves-devnet"
    }
  },
  packageSummary in Linux := "Waves node",
  packageDescription in Linux := "Waves node",
  debianPackageDependencies in Debian += "java8-runtime-headless",
  mappings in Universal += (baseDirectory.value / s"waves-$network.conf" -> "conf/waves.conf"),
  packageSource := sourceDirectory.value / "package",
  upstartScript := {
    val src = packageSource.value / "upstart.conf"
    val dest = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
    val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
    IO.write(dest, result)
    dest
  },
  linuxPackageMappings ++= Seq(
    packageMapping((upstartScript.value, s"/usr/share/${packageName.value}/conf/upstart.conf")),
    packageMapping((packageSource.value / "logback-overrides.xml", s"/usr/share/${packageName.value}/conf/logback-overrides.xml"))
  ).map(_.withConfig().withPerms("644").withUser(packageName.value).withGroup(packageName.value)),
  serviceAutostart in Debian := false,
  executableScriptName := packageName.value,
  maintainerScripts in Debian := maintainerScriptsFromDirectory(
    sourceDirectory.value / "package" / "debian",
    Seq("preinst", "postinst", "postrm", "prerm")
  )
)

lazy val waves = project.in(file("."))
  .enablePlugins(sbtdocker.DockerPlugin, JavaServerAppPackaging, JDebPackaging, SystemdPlugin)
  .settings(commonSettings ++ debianSettings)

//assembly settings
assemblyJarName in assembly := "waves.jar"
test in assembly := {}

