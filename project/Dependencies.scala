import sbt._

object Dependencies {

  lazy val testKit = Seq(
    "io.spray" %% "spray-testkit" % "1.+" % "test",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test"
  )

}
