resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.28")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.0"

Seq(
  "com.eed3si9n"       % "sbt-assembly"              % "0.14.10",
  "com.typesafe.sbt"   % "sbt-native-packager"       % "1.3.25",
  "org.scalastyle"     %% "scalastyle-sbt-plugin"    % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"             % "1.6.1",
  "se.marcuslonnberg"  % "sbt-docker"                % "1.8.1",
  "com.typesafe.sbt"   % "sbt-git"                   % "1.0.0",
  "org.scala-js"       % "sbt-scalajs"               % "0.6.33",
  "org.portable-scala" % "sbt-scalajs-crossproject"  % "0.6.0",
  "org.scalameta"      % "sbt-scalafmt"              % "2.2.1",
  "pl.project13.scala" % "sbt-jmh"                   % "0.3.3",
  "com.jsuereth"       % "sbt-pgp"                   % "1.1.1",
  "com.github.cb372"   % "sbt-explicit-dependencies" % "0.2.10"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.12.1",
  "org.hjson"                    % "hjson"                     % "3.0.0",
  "org.vafer"                    % "jdeb"                      % "1.5" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                    % "jcl-over-slf4j"            % "1.7.30",
  ("com.spotify" % "docker-client" % "8.15.1")
    .exclude("commons-logging", "commons-logging")
)
