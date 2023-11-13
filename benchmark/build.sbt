enablePlugins(JmhPlugin)

Jmh / version := "1.37"

libraryDependencies ++= Seq(
  ("org.scodec" %% "scodec-core" % "1.11.10").cross(CrossVersion.for3Use2_13),
  "org.eclipse.collections"                % "eclipse-collections"   % "11.1.0"
) ++ Dependencies.logDeps

// https://github.com/ktoso/sbt-jmh#adding-to-your-project
inConfig(Jmh)(
  Seq(
    sourceDirectory := (Test / sourceDirectory).value,
    classDirectory := (Test / classDirectory).value,
    dependencyClasspath := (Test / dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
    compile := compile.dependsOn(Test / compile).value,
    run := run.dependsOn(compile).evaluated
  )
)
