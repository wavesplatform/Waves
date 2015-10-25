name := "scorex-transaction"

libraryDependencies ++=
  Dependencies.testKit ++
  Dependencies.db ++
  Dependencies.serizalization ++
  Dependencies.logging ++
  Seq(
    "com.github.pathikrit" %% "better-files" % "2.13.0"
  )

