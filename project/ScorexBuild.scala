import sbt._
import Keys._

object ScorexBuild extends Build {

  lazy val buildSettings = Seq(
    organization := "org.consensusresearch",
    version := "1.1.0-SNAPSHOT",
    scalaVersion := "2.11.7"
  )

  def subModule(id: String): Project = Project(id = id, base = file(s"scorex-$id"))

  lazy val root = Project(id = "scorex", base = file("."))
    .aggregate(basics, transaction, consensus)
    .dependsOn(basics, transaction, consensus)

  lazy val basics = subModule("basics")

  lazy val transaction = subModule("transaction").aggregate(basics).dependsOn(basics)

  lazy val consensus = subModule("consensus").aggregate(basics).dependsOn(basics)
}
