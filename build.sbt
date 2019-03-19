import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys._
import sbt._
import sbtassembly.MergeStrategy
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtdocker.DockerPlugin

enablePlugins(JavaServerAppPackaging, JDebPackaging, SystemdPlugin, GitVersioning)
scalafmtOnCompile in ThisBuild := true
Global / cancelable := true
Global / coverageExcludedPackages := ".*"

val versionSource = Def.task {
  // WARNING!!!
  // Please, update the fallback version every major and minor releases.
  // This version is used then building from sources without Git repository
  // In case of not updating the version nodes build from headless sources will fail to connect to newer versions
  val FallbackVersion = (0, 16, 2)

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
val network = SettingKey[Network]("network")
network := { Network(sys.props.get("network")) }
name := "waves"
normalizedName := s"${name.value}${network.value.packageSuffix}"

git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")
logBuffered := false

inThisBuild(
  Seq(
    scalaVersion := "2.12.8",
    organization := "com.wavesplatform",
    crossPaths := false,
    dependencyOverrides ++= Dependencies.EnforcedVersions.value,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Ywarn-unused-import",
      "-Ypartial-unification"
    )
  ))

resolvers ++= Seq(
  Resolver.bintrayRepo("ethereum", "maven"),
  Resolver.bintrayRepo("dnvriend", "maven"),
  Resolver.sbtPluginRepo("releases")
)

val java9Options = Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",
  "--add-exports=java.base/jdk.internal.ref=ALL-UNNAMED"
)

fork in run := true
javaOptions in run ++= java9Options

Test / fork := true
Test / javaOptions ++= java9Options

Jmh / javaOptions ++= java9Options

val aopMerge: MergeStrategy = new MergeStrategy {
  val name = "aopMerge"
  import scala.xml._
  import scala.xml.dtd._

  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
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

inConfig(Test)(
  Seq(
    logBuffered := false,
    parallelExecution := false,
    testListeners := Seq.empty, // Fix for doubled test reports
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true")
  ))

inConfig(Linux)(
  Seq(
    maintainer := "wavesplatform.com",
    packageSummary := "Waves node",
    packageDescription := "Waves node"
  ))

bashScriptExtraDefines += s"""addJava "-Dwaves.directory=/var/lib/${normalizedName.value}""""

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

commands += Command.command("packageAll") { state =>
  "clean" :: "assembly" :: "debian:packageBin" :: state
}

addCommandAlias(
  "checkPR",
  // set scalacOptions in ThisBuild ++= Seq("-Xfatal-warnings");
  """;
    |Global / checkPRRaw;
    |set scalacOptions in ThisBuild -= "-Xfatal-warnings";
  """.stripMargin
)

lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw in Global := {
  try {
    clean.all(ScopeFilter(inAnyProject)).value
  } finally {
    test.all(ScopeFilter(inProjects(langJVM, node, commonJVM), inConfigurations(Test))).value
    (langJS / Compile / fastOptJS).value
    compile.all(ScopeFilter(inProjects(generator, benchmark), inConfigurations(Test))).value
  }
}

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .disablePlugins(ProtocPlugin)
  .settings(
    libraryDependencies += Dependencies.ScalaTest % "test"
  )

lazy val commonJS  = common.js
lazy val commonJVM = common.jvm

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .disablePlugins(ProtocPlugin)
    .dependsOn(common % "compile->compile;test->test")
    .settings(
      version := "1.0.0",
      coverageExcludedPackages := ".*",
      test in assembly := {},
      libraryDependencies ++=
        Dependencies.Lang.value ++
          Dependencies.test,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins"),
      resolvers += Resolver.sbtPluginRepo("releases")
    )
    .jsSettings(
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jvmSettings(
      coverageExcludedPackages := "",
      publishMavenStyle := true,
      credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
      publishTo := Some("Sonatype Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
      name := "RIDE Compiler",
      normalizedName := "lang",
      description := "The RIDE smart contract language compiler",
      homepage := Some(url("https://docs.wavesplatform.com/en/technical-details/waves-contracts-language-description/maven-compiler-package.html")),
      licenses := Seq(("MIT", url("https://github.com/wavesplatform/Waves/blob/master/LICENSE"))),
      organization := "com.wavesplatform",
      organizationName := "Waves Platform",
      organizationHomepage := Some(url("https://wavesplatform.com")),
      scmInfo := Some(ScmInfo(url("https://github.com/wavesplatform/Waves"), "git@github.com:wavesplatform/Waves.git", None)),
      developers := List(Developer("petermz", "Peter Zhelezniakov", "peterz@rambler.ru", url("https://wavesplatform.com"))),
      libraryDependencies ++=
        Seq(
          "org.scala-js"                      %% "scalajs-stubs" % "1.0.0-RC1" % "provided",
          "com.github.spullara.mustache.java" % "compiler"       % "0.9.5"
        )
    )

lazy val langJS  = lang.js
lazy val langJVM = lang.jvm

lazy val node = project
  .enablePlugins(ExtensionPackaging) //.configs(IntegrationTest)
  .settings(
    Compile / mainClass := Some("com.wavesplatform.Application"),
    libraryDependencies ++= Dependencies.Node.value, /*++ Dependencies.itKit.map(_ % IntegrationTest*/
    Compile / sourceGenerators += versionSource,
    Compile / PB.targets += scalapb.gen(flatPackage = true) -> (Compile / sourceManaged).value,
    Compile / PB.deleteTargetDirectory := false,
    Compile / publishArtifact in packageDoc := false,
    Compile / publishArtifact in packageSrc := false,
    coverageExcludedPackages := ""
  )
  .dependsOn(langJVM % "compile;test->test", commonJVM % "compile;test->test")

// To run integration tests from IDEA, enable the "sbt" checkbox
lazy val `node-it` = project
  .in(file(".") / "node" / "src" / "it")
  .enablePlugins(sbtdocker.DockerPlugin)
  .settings(ItSettings.settings ++ DockerSettings.settings)
  .settings(
    description := "NODE integration tests",
    target := (node / Compile / target).value / "it", // see ItSettings
    libraryDependencies ++= Dependencies.itTest,
    dependencyOverrides ++= Dependencies.EnforcedVersions.value,
    inTask(docker)(
      Seq(
        imageNames := Seq(ImageName("com.wavesplatform/node-it")),
        DockerSettings.additionalFiles ++= (node / Universal / stage).value +: Seq(
          (Test / resourceDirectory).value / "template.conf",
          sourceDirectory.value / "container" / "start-waves.sh"
        )
      ))
  )
  .dependsOn(node)

lazy val dex = project
  .settings(
    libraryDependencies ++= Dependencies.test
  )
  .dependsOn(
    node % "compile;test->test;runtime->provided"
  )

lazy val `dex-it` = project
  .in(file(".") / "dex" / "src" / "it")
  .enablePlugins(sbtdocker.DockerPlugin)
  .settings(ItSettings.settings ++ DockerSettings.settings)
  .settings(
    description := "DEX integration tests",
    target := (dex / Compile / target).value / "it", // see ItSettings
    libraryDependencies ++= Dependencies.itTest,
    dependencyOverrides ++= Dependencies.EnforcedVersions.value,
    inTask(docker)(
      Seq(
        imageNames := Seq(ImageName("com.wavesplatform/dex-it")),
        DockerSettings.additionalFiles ++= (`node-it` / docker / DockerSettings.additionalFiles).value ++ Seq(
          (dex / Universal / stage).value,
          (Test / resourceDirectory).value / "template.conf",
          sourceDirectory.value / "container" / "wallet.dat"
        )
      ))
  )
  .dependsOn(
    dex,
    `node-it` % "compile;test->test"
  )

lazy val generator = project
  .dependsOn(node % "compile->test")
  .settings(
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"
  )

lazy val dexgenerator = project
  .dependsOn(dex)
  .settings(
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"
  )

lazy val benchmark = project
  .enablePlugins(JmhPlugin)
  .dependsOn(node % "compile->compile;test->test", langJVM % "compile->compile;test->test")
