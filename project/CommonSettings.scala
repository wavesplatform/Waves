import sbt.Keys._
import sbt._

object CommonSettings extends AutoPlugin {
  object autoImport extends CommonKeys
  import autoImport._

  override def trigger: PluginTrigger = allRequirements

  // These options doesn't work for ScalaJS
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    packageSource := sourceDirectory.value / "package"
  )
}

trait CommonKeys {
  val network       = settingKey[Network]("The network for artifacts")
  val packageSource = settingKey[File]("Additional files for DEB")
}
