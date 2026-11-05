resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js. An RC because sbt-protoc has no stable sbt 2 build yet.
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.1.0-RC2")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "1.0.0-alpha.6"

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "2.4.2",
  "com.github.sbt"     % "sbt-git"                  % "3.0.0-RC1",
  "com.github.sbt"     % "sbt-native-packager"      % "1.11.7",
  "com.github.sbt"     % "sbt-pgp"                  % "2.3.2",
  "com.github.sbt"     % "sbt-javaagent"            % "0.3.0",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0",
  "org.scala-js"       % "sbt-scalajs"              % "1.22.0",
  "org.scalameta"      % "sbt-scalafmt"             % "2.6.2",
  "pl.project13.scala" % "sbt-jmh"                  % "0.4.8",
  "com.github.sbt"     % "sbt2-compat"              % "0.2.0"
).map(addSbtPlugin)

val dockerJavaVersion = "3.7.1"

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" %% "jackson-module-scala"              % "2.22.2",
  "org.hjson"                     % "hjson"                             % "3.1.0",
  ("org.vafer"                    % "jdeb"                              % "1.14").artifacts(Artifact("jdeb", "jar", "jar")),
  "org.slf4j"                     % "jcl-over-slf4j"                    % "2.0.18",
  "com.github.docker-java"        % "docker-java-core"                  % dockerJavaVersion,
  "com.github.docker-java"        % "docker-java-transport-httpclient5" % dockerJavaVersion
)
