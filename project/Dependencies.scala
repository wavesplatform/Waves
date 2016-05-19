import sbt._

object Dependencies {

  lazy val testKit = Seq(
    "com.typesafe.akka" %% "akka-testkit" % "2.+",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
  )

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "18.+",
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

  lazy val http = Seq(
    "com.typesafe.akka" %% "akka-http-experimental" % "2.+",
    "com.chuusai" %% "shapeless" % "2.+",
    "io.swagger" %% "swagger-scala-module" % "1.+",
    "io.swagger" % "swagger-core" % "1.+",
    "io.swagger" % "swagger-annotations" % "1.+",
    "io.swagger" % "swagger-models" % "1.+",
    "io.swagger" % "swagger-jaxrs" % "1.+",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.+"
  )
}
