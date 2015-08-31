import sbt._
import Keys._

object ScorexBuild extends Build {

  lazy val buildSettings = Seq(
    organization := "org.consensusresearch",
    version := "1.0.4",
    scalaVersion := "2.11.7"
  )

  def subModule(id: String): Project = Project(id = id, base = file(s"scorex-$id"))

  lazy val root = Project(id = "scorex", base = file(".")).aggregate(crypto).dependsOn(crypto)

  lazy val crypto = subModule("crypto")
}
