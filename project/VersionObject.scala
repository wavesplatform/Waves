import sbt.{AutoPlugin, Def}
import sbt.Keys._
import sbt._

object VersionObject extends AutoPlugin {
  object autoImport {
    object V {
      val scalaPackage = SettingKey[String]("version-scala-package", "Scala package name where Version object is created")
      val fallback     = SettingKey[(Int, Int, Int)]("version-fallback", "Version tuple to use when Git version is not available")
    }
  }

  import autoImport._

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    V.fallback := (0, 0, 1)
  )
  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    V.scalaPackage := s"${organization.value}.${name.value}",
    (Compile / sourceGenerators) += Def.task {
      val packageName = V.scalaPackage.value
      val versionFile = (Compile / sourceManaged).value / s"${packageName.replace('.', '/')}/Version.scala"

      val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
      val (major, minor, patch) = version.value match {
        case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
        case _                            => V.fallback.value
      }

      IO.write(
        versionFile,
        s"""package $packageName
           |
           |object Version {
           |  val VersionString = "${version.value}"
           |  val VersionTuple = ($major, $minor, $patch)
           |}
           |""".stripMargin
      )

      Seq(versionFile)
    }
  )
}
