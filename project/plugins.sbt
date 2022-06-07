resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.11"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "1.2.0",
  "com.github.sbt"     % "sbt-native-packager"      % "1.9.9",
  "se.marcuslonnberg"  % "sbt-docker"               % "1.9.0",
  "com.typesafe.sbt"   % "sbt-git"                  % "1.0.2",
  "org.scala-js"       % "sbt-scalajs"              % "1.10.0",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0",
  "pl.project13.scala" % "sbt-jmh"                  % "0.4.3",
  "com.github.sbt"     % "sbt-ci-release"           % "1.5.10"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.2",
  "org.hjson"                    % "hjson"                 % "3.0.0",
  "org.vafer"                    % "jdeb"                  % "1.10" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                    % "jcl-over-slf4j"        % "1.7.36",
  ("com.spotify" % "docker-client" % "8.16.0")
    .exclude("commons-logging", "commons-logging")
)
