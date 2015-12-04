name := "scorex-basics"

libraryDependencies ++=
    Dependencies.serizalization ++
    Dependencies.akka ++
    Dependencies.db ++
    Dependencies.spray ++
    Dependencies.testKit ++
    Dependencies.logging ++ Seq(
      "org.whispersystems" % "curve25519-java" % "+",
      "commons-net" % "commons-net" % "3.+"
  )