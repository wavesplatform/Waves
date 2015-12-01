name := "scorex-basics"

libraryDependencies ++=
    Dependencies.serizalization ++
    Dependencies.akka ++
    Dependencies.spray ++
    Dependencies.testKit ++
    Dependencies.db ++
    Dependencies.logging ++ Seq(
      "org.whispersystems" % "curve25519-java" % "+",
      "commons-net" % "commons-net" % "3.+"
  )