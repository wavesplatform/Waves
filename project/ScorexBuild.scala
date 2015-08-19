import sbt._
import Keys._

object ScorexBuild extends Build {

  def subModule(id: String): Project = Project(id = id, base = file(s"scorex-$id"))

  lazy val root = Project(id = "scorex", base = file(".")).aggregate(crypto).dependsOn(crypto)

  lazy val crypto = subModule("crypto")

}
