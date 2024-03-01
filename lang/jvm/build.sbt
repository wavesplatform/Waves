publishTo := sonatypePublishToBundle.value
publish / skip := false
homepage := Some(url("https://docs.waves.tech/en/ride/"))
developers := List(
  Developer("ismagin", "Ilya Smagin", "ilya.smagin@gmail.com", url("https://github.com/ismagin")),
  Developer("asayadyan", "Artyom Sayadyan", "xrtm000@gmail.com", url("https://github.com/xrtm000")),
  Developer("mpotanin", "Mike Potanin", "mpotanin@wavesplatform.com", url("https://github.com/potan")),
  Developer("irakitnykh", "Ivan Rakitnykh", "mrkr.reg@gmail.com", url("https://github.com/mrkraft"))
)

Compile / packageDoc / publishArtifact := true

inTask(assembly)(
  Seq(
    test            := {},
    assemblyJarName := s"file-compiler.jar",
    assemblyMergeStrategy := {
      case p if p.endsWith(".proto") || p.endsWith("module-info.class") || p.endsWith("io.netty.versions.properties") =>
        MergeStrategy.discard
      case "scala-collection-compat.properties" =>
        MergeStrategy.discard
      case p if Set("scala/util/control/compat", "scala/collection/compat").exists(p.replace('\\', '/').contains) =>
        MergeStrategy.last
      case other =>
        (assembly / assemblyMergeStrategy).value(other)
    }
  )
)
