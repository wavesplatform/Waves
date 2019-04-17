import WavesDockerKeys.exposedPorts
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbtassembly.MergeStrategy
import sbtdocker.DockerPlugin.autoImport.ImageName
import sbtdocker._

enablePlugins(JavaServerAppPackaging, UniversalDeployPlugin, JDebPackaging, SystemdPlugin, GitVersioning, sbtdocker.DockerPlugin)

val versionSource = Def.task {
  // WARNING!!!
  // Please, update the fallback version every major and minor releases.
  // This version is used then building from sources without Git repository
  // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
  val FallbackVersion = (0, 17, 1)

  val versionFile      = sourceManaged.value / "com" / "wavesplatform" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val (major, minor, patch) = version.value match {
    case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
    case _                            => FallbackVersion
  }
  IO.write(
    versionFile,
    s"""package com.wavesplatform
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $patch)
       |}
       |""".stripMargin
  )
  Seq(versionFile)
}

resolvers ++= Seq(
  Resolver.bintrayRepo("ethereum", "maven"),
  Resolver.bintrayRepo("dnvriend", "maven"),
  Resolver.sbtPluginRepo("releases")
)

libraryDependencies ++= Dependencies.node.value
coverageExcludedPackages := ""

scriptClasspath += "*" // adds "$lib_dir/*" to app_classpath in the executable file

inConfig(Compile)(
  Seq(
    mainClass := Some("com.wavesplatform.Application"),
    sourceGenerators += versionSource,
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

val network = SettingKey[Network]("network")
network := Network(sys.props.get("network"))

name := "waves"
normalizedName := s"${name.value}${network.value.packageSuffix}"

val linuxScriptPattern = "bin/(.+)".r
val batScriptPattern   = "bin/([^.]+)\\.bat".r

inConfig(Universal)(
  Seq(
    mappings += (baseDirectory.value / s"waves-${network.value}.conf" -> "doc/waves.conf.sample"),
    mappings := {
      val scriptSuffix = network.value.packageSuffix
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

inConfig(Linux)(
  Seq(
    maintainer := "wavesplatform.com",
    packageSummary := "Waves node",
    packageDescription := "Waves node"
  ))

bashScriptExtraDefines += s"""addJava "-Dwaves.directory=/var/lib/${normalizedName.value}""""

val packageSource = Def.setting {
  sourceDirectory.value / "package"
}

val upstartScript = Def.task {
  val src    = packageSource.value / "upstart.conf"
  val dest   = (target in Debian).value / "upstart" / s"${packageName.value}.conf"
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

inConfig(Debian)(
  Seq(
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java8-runtime-headless",
    serviceAutostart := false,
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm"))
  ))

inTask(docker) {
  Seq(
    imageNames := Seq(
      ImageName(
        namespace = Some("waves-node"),
        repository = "node-dev",
        tag = Some("latest")
      )),
    dockerfile := {
      val yourKitArchive = "YourKit-JavaProfiler-2019.1-docker.zip"
      val bin            = "/opt/waves/start-waves.sh"
      new Dockerfile {
        from("anapsix/alpine-java:8_server-jre")

        env("WAVES_CONFIG_FILE", "/opt/waves/waves.conf")

        runRaw(s"""mkdir -p /opt/waves && \\
                  |apk update && \\
                  |apk add --no-cache openssl ca-certificates && \\
                  |wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O /opt/waves/aspectjweaver.jar && \\
                  |wget --quiet "https://www.yourkit.com/download/docker/$yourKitArchive" -P /tmp/ && \\
                  |unzip /tmp/$yourKitArchive -d /usr/local && \\
                  |rm -f /tmp/$yourKitArchive""".stripMargin)

        add((Universal / stage).value, "/opt/waves")
        add((Default / sourceDirectory).value / "container" / "start-waves.sh", "/opt/waves")
        add((Default / sourceDirectory).value / "container" / "waves.conf", "/opt/waves")
        runShell("chmod", "+x", bin)
        entryPoint(bin)
        expose(6864, 6869)
      }
    }
  )
}
