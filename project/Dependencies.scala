import sbt._

object Dependencies {

  lazy val testKit = Seq(
    "io.spray" %% "spray-testkit" % "1.+" % "test",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test"
  )

  lazy val serizalization = Seq(
    "com.google.guava" % "guava" % "15.+",
    "com.typesafe.play" %% "play-json" % "2.+"
  )

  lazy val db = Seq(
    "org.mapdb" % "mapdb" % "1.+"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.+",
    "ch.qos.logback" % "logback-core" % "1.+"
  )
}
