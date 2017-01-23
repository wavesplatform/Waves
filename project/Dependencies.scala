import sbt._

object Dependencies {

  lazy val testKit = Seq(
    "com.typesafe.akka" %% "akka-testkit" % "2.+",
    "org.scalatest" %% "scalatest" % "2.+" % "test",
    "org.scalactic" %% "scalactic" % "2.+" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.+" % "test",
    "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
    "org.mockito" % "mockito-all" % "1.10.19" % "test"
  )

  lazy val serialization = Seq(
    "com.google.guava" % "guava" % "18.+",
    "com.typesafe.play" %% "play-json" % "2.4.+"
  )

  lazy val akka = Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.+",
    "com.typesafe.akka" %% "akka-slf4j" % "2.+"
  )

  lazy val p2p = Seq(
    "org.bitlet" % "weupnp" % "0.1.+"
  )

  lazy val db = Seq(
    "com.h2database" % "h2-mvstore" % "1.+"
  )

  lazy val logging = Seq(

    "ch.qos.logback" % "logback-classic" % "1.+",
    "ch.qos.logback" % "logback-core" % "1.+",
    "org.slf4j" % "slf4j-api" % "1.7.+",
    "org.slf4j" % "slf4j-nop" % "1.7.+" % "test"
  )

  lazy val http = Seq(
    "com.chuusai" %% "shapeless" % "2.+",
    "io.swagger" %% "swagger-scala-module" % "1.+",
    "io.swagger" % "swagger-core" % "1.+",
    "io.swagger" % "swagger-annotations" % "1.+",
    "io.swagger" % "swagger-models" % "1.+",
    "io.swagger" % "swagger-jaxrs" % "1.+",
    "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.+"
  )
}
