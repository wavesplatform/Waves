import sbt.*
import sbt.Keys.*
import xerial.sbt.Sonatype.autoImport.sonatypePublishToBundle

object PublishedModule extends AutoPlugin {
  override def projectSettings: Seq[Def.Setting[?]] = inConfig(Compile)(
    Seq(
      doc / sources                := Seq(),
      packageDoc / publishArtifact := true,
      packageDoc / mappings        := Seq(baseDirectory.value / "README.md" -> "README.md")
    )
  ) ++ Seq(
    publishTo      := sonatypePublishToBundle.value,
    publish / skip := false,
    Test / packageDoc / publishArtifact := false,
    versionScheme := Some("pvp")
  )
}
