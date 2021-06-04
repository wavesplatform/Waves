import CommonSettings.autoImport.network
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.Keys.executableScriptName
import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbtassembly.MergeStrategy

enablePlugins(RunApplicationSettings, JavaServerAppPackaging, UniversalDeployPlugin, JDebPackaging, SystemdPlugin, GitVersioning, VersionObject)

libraryDependencies ++= Dependencies.node.value
coverageExcludedPackages := ""

inConfig(Compile)(
  Seq(
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
    PB.deleteTargetDirectory := false,
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false
  )
)

inTask(assembly)(
  Seq(
    test := {},
    assemblyJarName := s"waves-all-${version.value}.jar",
    assemblyMergeStrategy := {
      case "module-info.class"                                  => MergeStrategy.discard
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.concat
      case other                                                => (assemblyMergeStrategy in assembly).value(other)
    }
  )
)

// Adds "$lib_dir/*" to app_classpath in the executable file
// Logback creates a "waves.directory_UNDEFINED" without this option.
scriptClasspath += "*"

bashScriptExtraDefines ++= Seq(
  s"""addJava "-Dwaves.defaults.blockchain.type=${network.value}"""",
  s"""addJava "-Dwaves.defaults.directory=/var/lib/${(Universal / normalizedName).value}"""",
  s"""addJava "-Dwaves.defaults.config.directory=/etc/${(Universal / normalizedName).value}"""",
  // Workaround to ignore the -h option
  """process_args() {
    |  local no_more_snp_opts=0
    |  while [[ $# -gt 0 ]]; do
    |    case "$1" in
    |    --) shift && no_more_snp_opts=1 && break ;;
    |    -no-version-check) no_version_check=1 && shift ;;
    |    -java-home) require_arg path "$1" "$2" && jre=$(eval echo $2) && java_cmd="$jre/bin/java" && shift 2 ;;
    |    -D* | -agentlib* | -XX*) addJava "$1" && shift ;;
    |    -J*) addJava "${1:2}" && shift ;;
    |    *) addResidual "$1" && shift ;;
    |    esac
    |  done
    |
    |  if [[ no_more_snp_opts ]]; then
    |    while [[ $# -gt 0 ]]; do
    |      addResidual "$1" && shift
    |    done
    |  fi
    |
    |  is_function_defined process_my_args && {
    |    myargs=("${residual_args[@]}")
    |    residual_args=()
    |    process_my_args "${myargs[@]}"
    |  }
    |}
    |""".stripMargin
)

inConfig(Universal)(
  Seq(
    mappings += (baseDirectory.value / s"waves-sample.conf" -> "doc/waves.conf.sample"),
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
      "-J-Xmx2g",
      "-J-XX:+ExitOnOutOfMemoryError",
      "-J-XX:+UseG1GC",
      "-J-XX:+ParallelRefProcEnabled",
      "-J-XX:+UseStringDeduplication",
      // JVM default charset for proper and deterministic getBytes behaviour
      "-J-Dfile.encoding=UTF-8"
    )
  )
)

inConfig(Linux)(
  Seq(
    maintainer := "wavesplatform.com",
    packageSummary := "Waves node",
    packageDescription := "Waves node"
  )
)

// Variable options are used in different tasks and configs, so we will specify all of them
val nameFix = Seq(
  name := "waves",
  packageName := s"${name.value}${network.value.packageSuffix}",
  normalizedName := s"${name.value}${network.value.packageSuffix}"
)

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
  ) ++ nameFix
)

V.scalaPackage := "com.wavesplatform"

// Hack for https://youtrack.jetbrains.com/issue/SCL-15210

moduleName := s"waves${network.value.packageSuffix}" // waves-*.jar instead of node-*.jar
executableScriptName := moduleName.value             // bin/waves instead of bin/node

nameFix
inScope(Global)(nameFix)
