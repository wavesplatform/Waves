logLevel := Level.Warn

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "http://repo.artima.com/releases"
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.2.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M7")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "+")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")

addSbtPlugin("se.marcuslonnberg" % "sbt-docker" % "1.4.1")

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.0",
  "com.spotify" % "docker-client" % "8.1.2",
  "org.vafer" % "jdeb" % "1.5" artifacts Artifact("jdeb", "jar", "jar"))
