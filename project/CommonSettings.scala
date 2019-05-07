import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.isScalaJSProject
import sbt.Keys._
import sbt._

object CommonSettings extends AutoPlugin {
  object autoImport extends CommonKeys
  import autoImport._

  override def trigger: PluginTrigger = allRequirements

  // These options doesn't work for ScalaJS
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    network := Network(sys.props.get("network")),
    javaOptions ++= {
      if (isScalaJSProject.value || !fork.value) Seq.empty else ModernJavaSettings.options
    }
  )
}

trait CommonKeys {
  val network = SettingKey[Network]("network")
}
