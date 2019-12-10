enablePlugins(JmhPlugin)

libraryDependencies += "org.scodec" %% "scodec-core" % "1.10.3"

inTask(Compile / run)(
  Seq(
    fork := true,
    javaOptions += s"-Dlogback.configurationFile=${(Compile / resourceDirectory).value / "logback.xml"}"
  ))

// https://github.com/ktoso/sbt-jmh#adding-to-your-project
inConfig(Jmh)(
  Seq(
    sourceDirectory := (Test / sourceDirectory).value,
    classDirectory := (Test / classDirectory).value,
    dependencyClasspath := (Test / dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
    compile := compile.dependsOn(Test / compile).value,
    run := run.dependsOn(compile).evaluated
  ))
