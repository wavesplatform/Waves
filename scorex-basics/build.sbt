name := "scorex-basics"

libraryDependencies ++=
    Dependencies.serizalization ++
    Dependencies.akka ++
    Dependencies.spray ++
    Dependencies.testKit ++
    Dependencies.logging ++ Seq(
      "net.vrallev.ecc" % "ecc-25519-java" % "+",
      "commons-net" % "commons-net" % "3.+"
  )