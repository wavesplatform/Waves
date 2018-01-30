
version := "0.1"

//scalacOptions in ThisBuild += "-Xlog-implicits"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "io.monix" %% "monix" % "3.0.0-M3",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scalacheck" %% "scalacheck" % "1.13.5",
  "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.3",
  "org.scalactic" %% "scalactic" % "3.0.3"
)

libraryDependencies ++= Seq(
  ("org.scorexfoundation" %% "scrypto" % "1.2.2").exclude("org.slf4j", "slf4j-api")
)