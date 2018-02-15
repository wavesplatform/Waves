import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.1",
      "io.monix" %% "monix" % "3.0.0-M3",
      "org.scodec" %%% "scodec-core" % "1.10.3",
      "org.scalacheck" %% "scalacheck" % "1.13.5",
      //"com.lihaoyi" %% "fastparse" % "1.0.0",
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % Test
    ) ++ Seq(
      "org.scalatest" %% "scalatest" % "3.0.3",
      "org.scalactic" %% "scalactic" % "3.0.3"
    ) ++ Seq(
      ("org.scorexfoundation" %% "scrypto" % "1.2.2").exclude("org.slf4j", "slf4j-api")
    ))
    .jsSettings(

    ) // defined in sbt-scalajs-crossproject
    .jvmSettings(libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "0.6.22" % "provided")
    .in(file("."))

lazy val langJS = lang.js
lazy val langJVM = lang.jvm