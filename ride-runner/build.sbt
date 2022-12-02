libraryDependencies ++= Seq(
  "com.thesamet.scalapb"         %% "scalapb-json4s" % "0.11.1",
  "com.github.ben-manes.caffeine" % "caffeine"       % "3.1.2",
  Dependencies.sttp3,
  Dependencies.sttp3Monix,
  Dependencies.leveldbJava().exclude("com.google.guava", "guava") % Test,
  Dependencies.akkaHttpModule("akka-http-testkit")                % Test
) ++ Dependencies.logDeps ++ Dependencies.test

run / fork := true
