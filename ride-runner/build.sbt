libraryDependencies ++= Seq(
  "com.thesamet.scalapb"                                         %% "scalapb-json4s" % "0.11.1",
  Dependencies.leveldbJava().exclude("com.google.guava", "guava") % Test
) ++ Dependencies.logDeps ++ Dependencies.test
