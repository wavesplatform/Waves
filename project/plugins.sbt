resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.8")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "1.0.0-alpha.3"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "2.3.1",
  "com.github.sbt"     % "sbt-git"                  % "2.1.0",
  "com.github.sbt"     % "sbt-native-packager"      % "1.11.3",
  "com.github.sbt"     % "sbt-pgp"                  % "2.3.1",
  "com.lightbend.sbt"  % "sbt-javaagent"            % "0.1.6",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2",
  "org.scala-js"       % "sbt-scalajs"              % "1.20.1",
  "org.scalameta"      % "sbt-scalafmt"             % "2.5.5",
  "pl.project13.scala" % "sbt-jmh"                  % "0.4.8"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.20.0",
  "org.hjson"                     % "hjson"                % "3.1.0",
  "org.vafer"                     % "jdeb"                 % "1.14" artifacts Artifact("jdeb", "jar", "jar"),
  "org.slf4j"                     % "jcl-over-slf4j"       % "2.0.17",
  ("com.spotify"                  % "docker-client"        % "8.16.0")
    .exclude("commons-logging", "commons-logging")
)
