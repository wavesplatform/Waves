resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org"
)

Seq(
  "com.eed3si9n" % "sbt-assembly" % "0.14.3",
  "com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M8",
  "org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0",
  "org.scoverage" % "sbt-scoverage" % "1.5.0",
  "com.github.gseitz" % "sbt-release" % "1.0.3",
  "se.marcuslonnberg" % "sbt-docker" % "1.4.1",
  "com.typesafe.sbt" % "sbt-git" % "0.9.3"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.0",
  "org.vafer" % "jdeb" % "1.5" artifacts Artifact("jdeb", "jar", "jar"))
