name := "scorex-crypto"

libraryDependencies ++=
  Dependencies.testKit ++
    Dependencies.serizalization ++
    Dependencies.logging ++ Seq(
    "commons-net" % "commons-net" % "3.+"
  )

ScorexBuild.buildSettings