import sbt.Keys._
import sbt._

object RunApplicationSettings extends AutoPlugin {
  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(
      Seq(
        mainClass := Some("com.wavesplatform.Application"),
        discoveredMainClasses := (mainClass in Compile).value.toSeq,
        run / fork := true
      ))
}
