resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.2.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.6")

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

libraryDependencies += "org.vafer" % "jdeb" % "1.3" artifacts Artifact("jdeb", "jar", "jar")
