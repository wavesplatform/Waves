libraryDependencies ++= Seq(
  Dependencies.scalaLogging,
  Dependencies.googleGuava,
  Dependencies.monixModule("reactive").value,
  Dependencies.curve25519
) ++ Dependencies.logDeps

enablePlugins(JavaAppPackaging)
