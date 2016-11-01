// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.2.3")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")

//Note: If using ScalaTest 2.2.6 or earlier, use SuperSafe version 1.1.0-RC6 instead, which will be the last version of SuperSafe to support ScalaTest 2.x.
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0-RC6")

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

