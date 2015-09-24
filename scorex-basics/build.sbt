name := "scorex-basics"

libraryDependencies ++=
    Dependencies.serizalization ++
    Dependencies.akka ++
    Dependencies.spray ++
    Dependencies.testKit ++
    Dependencies.logging ++ Seq(
    "commons-net" % "commons-net" % "3.+"
  )