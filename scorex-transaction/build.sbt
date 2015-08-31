name := "scorex-transaction"

libraryDependencies ++= Dependencies.testKit ++ Dependencies.db ++
  Dependencies.serizalization ++ Dependencies.logging

ScorexBuild.buildSettings
