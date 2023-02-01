ThisProject / publishTo := sonatypePublishToBundle.value
ThisProject / publish / skip := false

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
