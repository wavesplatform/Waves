import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.isScalaJSProject
import sbt.Keys._
import sbt._

object CommonSettings extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  // These options doesn't work for ScalaJS
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    javaOptions ++= {
      if (isScalaJSProject.value) Seq.empty else ModernJavaSettings.options
    }
  )
}
