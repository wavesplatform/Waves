resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.7")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.17"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "2.2.0",
  "com.github.sbt"     % "sbt-native-packager"      % "1.10.4",
  "se.marcuslonnberg"  % "sbt-docker"               % "1.11.0",
  "org.scala-js"       % "sbt-scalajs"              % "1.16.0",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2",
  "pl.project13.scala" % "sbt-jmh"                  % "0.4.7",
  "com.github.sbt"     % "sbt-ci-release"           % "1.6.1",
  "com.lightbend.sbt"  % "sbt-javaagent"            % "0.1.6"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.2",
  "org.hjson"                     % "hjson"                % "3.1.0",
  "org.vafer"                     % "jdeb"                 % "1.11" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                     % "jcl-over-slf4j"       % "2.0.16",
  ("com.spotify"                  % "docker-client"        % "8.16.0")
    .exclude("commons-logging", "commons-logging")
)
