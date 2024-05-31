enablePlugins(
  RunApplicationSettings,
  JavaServerAppPackaging,
  UniversalDeployPlugin,
  JDebPackaging,
  SystemdPlugin,
  VersionObject,
  JavaAgent
)

libraryDependencies ++= Dependencies.node.value

javaAgents ++= {
  if (instrumentation.value) {
    Dependencies.kanela
  } else {
    Seq.empty
  }
}

publishTo      := sonatypePublishToBundle.value
publish / skip := false
homepage       := Some(url("https://waves.tech/"))
developers := List(
  Developer("ismagin", "Ilya Smagin", "ilya.smagin@gmail.com", url("https://github.com/ismagin")),
  Developer("asayadyan", "Artyom Sayadyan", "xrtm000@gmail.com", url("https://github.com/xrtm000")),
  Developer("mpotanin", "Mike Potanin", "mpotanin@wavesplatform.com", url("https://github.com/potan")),
  Developer("irakitnykh", "Ivan Rakitnykh", "mrkr.reg@gmail.com", url("https://github.com/mrkraft")),
  Developer("akiselev", "Alexey Kiselev", "alexey.kiselev@gmail.com>", url("https://github.com/alexeykiselev")),
  Developer("phearnot", "Sergey Nazarov", "snazarov@web3tech.ru", url("https://github.com/phearnot")),
  Developer("tolsi", "Sergey Tolmachev", "tolsi.ru@gmail.com", url("https://github.com/tolsi")),
  Developer("vsuharnikov", "Vyatcheslav Suharnikov", "arz.freezy@gmail.com", url("https://github.com/vsuharnikov")),
  Developer("ivan-mashonskiy", "Ivan Mashonskii", "ivan.mashonsky@gmail.com", url("https://github.com/ivan-mashonskiy"))
)
versionScheme := Some("pvp")

Compile / packageDoc / publishArtifact := true
Test / packageDoc / publishArtifact    := false

inConfig(Compile)(
  Seq(
    PB.targets += scalapb.gen(flatPackage = true) -> sourceManaged.value,
    PB.protoSources += PB.externalIncludePath.value,
    PB.generate / includeFilter := { (f: File) =>
      (** / "waves" / "*.proto").matches(f.toPath)
    },
    PB.deleteTargetDirectory := false
  )
)

inTask(assembly)(Seq(name := "waves") ++ CommonSettings.assemblySettings)

// Adds "$lib_dir/*" to app_classpath in the executable file, this is needed for extensions
scriptClasspath += "*"

bashScriptExtraDefines +=
  """# Workaround to ignore the -h option
    |process_args() {
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

bashScriptExtraDefines += bashScriptEnvConfigLocation.value.fold("")(envFile => s"[[ -f $envFile ]] && . $envFile")

linuxScriptReplacements += ("network" -> network.value.toString)

inConfig(Universal)(
  Seq(
    packageName                                             := s"waves-${version.value}",
    mappings += (baseDirectory.value / s"waves-sample.conf" -> "doc/waves.conf.sample"),
    javaOptions ++= Seq(
      // -J prefix is required by the bash script
      "-J-server",
      "-J-Xmx2g",
      "-J-XX:+ExitOnOutOfMemoryError",
      "-J-XX:+UseG1GC",
      "-J-XX:+ParallelRefProcEnabled",
      "-J-XX:+UseStringDeduplication",
      // JVM default charset for proper and deterministic getBytes behaviour
      "-J-Dfile.encoding=UTF-8",
      "-J--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
    )
  )
)

inConfig(Linux)(
  Seq(
    packageSummary     := "Waves node",
    packageDescription := "Waves node",
    name               := s"waves${network.value.packageSuffix}",
    normalizedName     := name.value,
    packageName        := normalizedName.value
  )
)

def fixScriptName(path: String, name: String, packageName: String): String =
  path.replace(s"/bin/$name", s"/bin/$packageName")

linuxPackageMappings := linuxPackageMappings.value.map { lpm =>
  lpm.copy(mappings = lpm.mappings.map {
    case (file, path) if path.endsWith(s"/bin/${name.value}") => file -> fixScriptName(path, name.value, (Linux / packageName).value)
    case (file, path) if path.endsWith("/conf/application.ini") =>
      val dest = (Debian / target).value / path
      IO.write(
        dest,
        s"""-J-Dwaves.defaults.blockchain.type=${network.value}
           |-J-Dwaves.defaults.directory=/var/lib/${(Linux / packageName).value}
           |-J-Dwaves.defaults.config.directory=/etc/${(Linux / packageName).value}
           |""".stripMargin
      )
      IO.append(dest, IO.readBytes(file))
      dest -> path
    case other => other
  })
}

linuxPackageSymlinks := linuxPackageSymlinks.value.map { lsl =>
  if (lsl.link.endsWith(s"/bin/${name.value}"))
    lsl.copy(
      fixScriptName(lsl.link, name.value, (Linux / packageName).value),
      fixScriptName(lsl.destination, name.value, (Linux / packageName).value)
    )
  else lsl
}

inConfig(Debian)(
  Seq(
    maintainer               := "com.wavesplatform",
    packageSource            := sourceDirectory.value / "package",
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java11-runtime-headless",
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("postinst", "postrm", "prerm"))
  )
)

V.scalaPackage := "com.wavesplatform"
