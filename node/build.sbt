import CommonSettings.autoImport.network
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.Keys.executableScriptName
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbtassembly.MergeStrategy

enablePlugins(RunApplicationSettings, JavaServerAppPackaging, UniversalDeployPlugin, JDebPackaging, SystemdPlugin, GitVersioning, VersionObject)

resolvers ++= Seq(
  Resolver.bintrayRepo("ethereum", "maven"),
  Resolver.bintrayRepo("dnvriend", "maven"),
  Resolver.sbtPluginRepo("releases")
)

libraryDependencies ++= Dependencies.node.value
coverageExcludedPackages := ""

inConfig(Compile)(
  Seq(
    PB.protoSources in Compile := Seq(PB.externalIncludePath.value, sourceDirectory.value / "protobuf"),
    includeFilter in PB.generate := new SimpleFileFilter((f: File) => f.getName.endsWith(".proto") && f.getParent.endsWith("waves")),
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
    PB.deleteTargetDirectory := false,
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false
  ))

val aopMerge: MergeStrategy = new MergeStrategy {
  import scala.xml._
  import scala.xml.dtd._

  override val name = "aopMerge"
  override def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val dt                         = DocType("aspectj", PublicID("-//AspectJ//DTD//EN", "http://www.eclipse.org/aspectj/dtd/aspectj.dtd"), Nil)
    val file                       = MergeStrategy.createMergeTarget(tempDir, path)
    val xmls: Seq[Elem]            = files.map(XML.loadFile)
    val aspectsChildren: Seq[Node] = xmls.flatMap(_ \\ "aspectj" \ "aspects" \ "_")
    val weaverChildren: Seq[Node]  = xmls.flatMap(_ \\ "aspectj" \ "weaver" \ "_")
    val options: String            = xmls.map(x => (x \\ "aspectj" \ "weaver" \ "@options").text).mkString(" ").trim
    val weaverAttr                 = if (options.isEmpty) Null else new UnprefixedAttribute("options", options, Null)
    val aspects                    = new Elem(null, "aspects", Null, TopScope, false, aspectsChildren: _*)
    val weaver                     = new Elem(null, "weaver", weaverAttr, TopScope, false, weaverChildren: _*)
    val aspectj                    = new Elem(null, "aspectj", Null, TopScope, false, aspects, weaver)
    XML.save(file.toString, aspectj, "UTF-8", xmlDecl = false, dt)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

inTask(assembly)(
  Seq(
    test := {},
    assemblyJarName := s"waves-all-${version.value}.jar",
    assemblyMergeStrategy := {
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat
      case PathList("META-INF", "aop.xml")                      => aopMerge
      case other                                                => (assemblyMergeStrategy in assembly).value(other)
    }
  ))

scriptClasspath += "*" // adds "$lib_dir/*" to app_classpath in the executable file
// Logback creates a "waves.directory_UNDEFINED" without this option.
bashScriptExtraDefines ++= Seq(
  s"""addJava "-Dwaves.defaults.directory=/var/lib/${(Universal / normalizedName).value}"""",
  s"""addJava "-Dwaves.defaults.config.directory=/etc/${(Universal / normalizedName).value}""""
)

inConfig(Universal)(
  Seq(
    mappings += (baseDirectory.value / s"waves-${network.value}.conf" -> "doc/waves.conf.sample"),
    mappings := {
      val linuxScriptPattern = "bin/(.+)".r
      val batScriptPattern   = "bin/([^.]+)\\.bat".r
      val scriptSuffix       = network.value.packageSuffix
      mappings.value.map {
        case m @ (file, batScriptPattern(script)) =>
          if (script.endsWith(scriptSuffix)) m else (file, s"bin/$script$scriptSuffix.bat")
        case m @ (file, linuxScriptPattern(script)) =>
          if (script.endsWith(scriptSuffix)) m else (file, s"bin/$script$scriptSuffix")
        case other => other
      }
    },
    javaOptions ++= Seq(
      // -J prefix is required by the bash script
      "-J-server",
      // JVM memory tuning for 2g ram
      "-J-Xms128m",
      "-J-Xmx2g",
      "-J-XX:+ExitOnOutOfMemoryError",
      // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
      "-J-XX:+UseG1GC",
      "-J-XX:+UseNUMA",
      "-J-XX:+AlwaysPreTouch",
      // probably can't use these with jstack and others tools
      "-J-XX:+PerfDisableSharedMem",
      "-J-XX:+ParallelRefProcEnabled",
      "-J-XX:+UseStringDeduplication",
      // JVM default charset for proper and deterministic getBytes behaviour
      "-J-Dfile.encoding=UTF-8"
    )
  ))

inConfig(Linux)(
  Seq(
    maintainer := "wavesplatform.com",
    packageSummary := "Waves node",
    packageDescription := "Waves node"
  ))

inConfig(Debian)(
  Seq(
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java8-runtime-headless",
    serviceAutostart := false,
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm")),
    linuxPackageMappings ++= {
      val upstartScript = {
        val src    = packageSource.value / "upstart.conf"
        val dest   = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
        val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
        IO.write(dest, result)
        dest
      }

      Seq(upstartScript -> s"/etc/init/${packageName.value}.conf").map(packageMapping(_).withConfig().withPerms("644"))
    },
    linuxScriptReplacements += "detect-loader" ->
      """is_systemd() {
        |    which systemctl >/dev/null 2>&1 && \
        |    systemctl | grep -- -\.mount >/dev/null 2>&1
        |}
        |is_upstart() {
        |    /sbin/init --version | grep upstart >/dev/null 2>&1
        |}
        |""".stripMargin
  ) ++ nameFix)

V.scalaPackage := "com.wavesplatform"

// Hack for https://youtrack.jetbrains.com/issue/SCL-15210

moduleName := s"waves${network.value.packageSuffix}" // waves-*.jar instead of node-*.jar
executableScriptName := moduleName.value // bin/waves instead of bin/node

// Variable options are used in different tasks and configs, so we will specify all of them
val nameFix = Seq(
  name := "waves",
  packageName := s"${name.value}${network.value.packageSuffix}",
  normalizedName := s"${name.value}${network.value.packageSuffix}"
)

nameFix
inScope(Global)(nameFix)
