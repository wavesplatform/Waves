import sbt.*
import sbt.Keys.*
import sbtcompat.PluginCompat.toFileRef

object PublishedModule extends AutoPlugin {
  override def projectSettings: Seq[Def.Setting[?]] = inConfig(Compile)(
    Seq(
      doc / sources                := Seq(),
      packageDoc / publishArtifact := true,
      packageDoc / mappings := {
        implicit val conv: xsbti.FileConverter = fileConverter.value
        Seq(toFileRef(baseDirectory.value / "README.md") -> "README.md")
      }
    )
  ) ++ Seq(
    publish / skip := false,
    Test / packageDoc / publishArtifact := false,
    versionScheme := Some("pvp")
  )
}
