import sbt.*
import sbt.Keys.{test, version, name}
import sbtassembly.AssemblyKeys.{assembly, assemblyMergeStrategy}
import sbtassembly.AssemblyPlugin.autoImport.assemblyJarName
import sbtassembly.{MergeStrategy, PathList}

object CommonSettings extends AutoPlugin {
  object autoImport extends CommonKeys

  override def trigger: PluginTrigger = allRequirements

  // These options doesn't work for ScalaJS
  override def projectSettings: Seq[Def.Setting[?]] = Seq()

  val assemblySettings: Seq[Def.Setting[?]] = Seq(
    assemblyJarName := s"${name.value}-all-${version.value}.jar",
    test := {},
    assemblyMergeStrategy := {
    case p
        if p.endsWith(".proto") ||
          p.endsWith("module-info.class") ||
          p.endsWith("io.netty.versions.properties") ||
          p.endsWith(".kotlin_module") =>
      MergeStrategy.discard

    case "scala-collection-compat.properties" =>
      MergeStrategy.discard

    case "logback.xml" | PathList("scala", "util", "control", "compat") | PathList("scala", "collection", "compat") |
        PathList("swagger-ui", "openapi.yaml") =>
      MergeStrategy.last
    case other => (assembly / assemblyMergeStrategy).value(other)
  })
}

trait CommonKeys {
  val network         = settingKey[Network]("The network for artifacts")
  val packageSource   = settingKey[File]("Additional files for DEB")
  val instrumentation = settingKey[Boolean]("Include kanela java agent in start script")
}
