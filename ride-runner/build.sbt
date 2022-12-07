libraryDependencies ++= Seq(
  "com.thesamet.scalapb"         %% "scalapb-json4s"                % "0.11.1",
  // https://github.com/netty/netty/wiki/Native-transports
  "io.netty"                      % "netty-transport-native-epoll"  % "4.1.79.Final" classifier "linux-x86_64",
  "com.github.ben-manes.caffeine" % "caffeine"                      % "3.1.2",
  Dependencies.sttp3,
  Dependencies.sttp3Monix,
  Dependencies.leveldbJava().exclude("com.google.guava", "guava") % Test,
  Dependencies.akkaHttpModule("akka-http-testkit")                % Test
) ++ Dependencies.logDeps ++ Dependencies.test

run / fork := true

enablePlugins(
  JavaServerAppPackaging,
  UniversalDeployPlugin
)
