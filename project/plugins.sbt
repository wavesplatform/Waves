resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org",
  Resolver.sbtPluginRepo("releases")
)

// Should go before Scala.js
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.19")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.8.4"

Seq(
  "com.eed3si9n"       % "sbt-assembly"              % "0.14.5",
  "com.typesafe.sbt"   % "sbt-native-packager"       % "1.3.25",
  "org.scalastyle"     %% "scalastyle-sbt-plugin"    % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"             % "1.5.1",
  "se.marcuslonnberg"  % "sbt-docker"                % "1.4.1",
  "com.typesafe.sbt"   % "sbt-git"                   % "1.0.0",
  "org.scala-js"       % "sbt-scalajs"               % "0.6.31",
  "org.portable-scala" % "sbt-scalajs-crossproject"  % "0.6.0",
  "org.scalameta"      % "sbt-scalafmt"              % "2.2.1",
  "pl.project13.scala" % "sbt-jmh"                   % "0.3.3",
  "com.jsuereth"       % "sbt-pgp"                   % "1.1.1",
  "com.github.cb372"   % "sbt-explicit-dependencies" % "0.2.10"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.9.9",
  "org.hjson"                    % "hjson"                     % "3.0.0",
  "org.vafer"                    % "jdeb"                      % "1.5" artifacts Artifact("jdeb", "jar", "jar")
)
