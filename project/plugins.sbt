resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.3")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.3"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "0.14.10",
  "com.github.sbt"     % "sbt-native-packager"      % "1.9.1",
  "org.scalastyle"     %% "scalastyle-sbt-plugin"   % "1.0.0",
  "se.marcuslonnberg"  % "sbt-docker"               % "1.8.2",
  "com.typesafe.sbt"   % "sbt-git"                  % "1.0.0",
  "org.scala-js"       % "sbt-scalajs"              % "1.5.1",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0",
  "org.scalameta"      % "sbt-scalafmt"             % "2.2.1",
  "pl.project13.scala" % "sbt-jmh"                  % "0.3.7",
  "com.jsuereth"       % "sbt-pgp"                  % "1.1.1"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.1",
  "org.hjson"                    % "hjson"                     % "3.0.0",
  "org.vafer"                    % "jdeb"                      % "1.5" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                    % "jcl-over-slf4j"            % "1.7.30",
  ("com.spotify" % "docker-client" % "8.16.0")
    .exclude("commons-logging", "commons-logging")
)
