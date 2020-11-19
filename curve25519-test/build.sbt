libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  Dependencies.googleGuava,
  Dependencies.monixModule("reactive").value,
  Dependencies.curve25519
) ++ Dependencies.logDeps

enablePlugins(JavaAppPackaging)
