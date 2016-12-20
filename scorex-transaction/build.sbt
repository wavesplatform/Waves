name := "scorex-transaction"

libraryDependencies ++=
  Dependencies.testKit ++
  Dependencies.db ++
  Dependencies.serialization ++
  Dependencies.logging ++
  Seq(
    "com.github.pathikrit" %% "better-files" % "2.13.0",
    "org.typelevel" %% "cats" % "0.8.1"
  )

