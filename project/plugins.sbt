resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.14"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "2.1.5",
  "com.github.sbt"     % "sbt-native-packager"      % "1.9.16",
  "se.marcuslonnberg"  % "sbt-docker"               % "1.11.0",
  "org.scala-js"       % "sbt-scalajs"              % "1.14.0",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2",
  "pl.project13.scala" % "sbt-jmh"                  % "0.4.6",
  "com.github.sbt"     % "sbt-ci-release"           % "1.5.12",
  "com.lightbend.sbt"  % "sbt-javaagent"            % "0.1.6"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.16.0",
  "org.hjson"                     % "hjson"                % "3.1.0",
  "org.vafer"                     % "jdeb"                 % "1.10" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                     % "jcl-over-slf4j"       % "2.0.9",
  ("com.spotify"                  % "docker-client"        % "8.16.0")
    .exclude("commons-logging", "commons-logging")
)
