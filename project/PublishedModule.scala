import sbt.*
import sbt.Keys.*

object PublishedModule extends AutoPlugin {
  override def projectSettings: Seq[Def.Setting[?]] = inConfig(Compile)(
    Seq(
      doc / sources                := Seq(),
      packageDoc / publishArtifact := true,
      packageDoc / mappings        := Seq(baseDirectory.value / "README.md" -> "README.md")
    )
  ) ++ Seq(
    publish / skip := false,
    Test / packageDoc / publishArtifact := false,
    versionScheme := Some("pvp")
  )
}
