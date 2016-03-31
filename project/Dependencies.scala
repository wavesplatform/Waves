import sbt._

object Dependencies {

  lazy val testKit = Seq(
    "io.spray" %% "spray-testkit" % "1.+" % "test",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
  )

  lazy val serizalization = Seq(
    "com.google.guava" % "guava" % "15.+",
    "com.typesafe.play" %% "play-json" % "2.4.+"
  )

  lazy val akka = Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.+"
  )

  lazy val p2p = Seq(
    "org.bitlet" % "weupnp" % "0.1.+"
  )

  lazy val db = Seq(
    "com.h2database" % "h2-mvstore" % "1.+",
    "org.mapdb" % "mapdb" % "2.+"
  )

  lazy val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.+",
    "ch.qos.logback" % "logback-core" % "1.+"
  )

  lazy val spray = Seq(
    "io.spray" %% "spray-routing" % "1.+",
    "io.spray" %% "spray-can" % "1.+",
    "io.spray" %% "spray-http" % "1.+",
    "io.spray" %% "spray-httpx" % "1.+",
    "io.spray" %% "spray-util" % "1.+",
    "com.gettyimages" %% "spray-swagger" % "0.+"
  )
}
