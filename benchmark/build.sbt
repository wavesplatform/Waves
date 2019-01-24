// https://github.com/ktoso/sbt-jmh#adding-to-your-project
inConfig(Jmh)(
  Seq(
    sourceDirectory := (Test / sourceDirectory).value,
    classDirectory := (Test / classDirectory).value,
    dependencyClasspath := (Test / dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
    compile := compile.dependsOn(Test / compile).value,
    run := run.dependsOn(Keys.compile).evaluated
  ))

Compile / run / fork := true
Compile / run / javaOptions ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",
  s"-Dlogback.configurationFile=${(Compile / resourceDirectory).value / "logback.xml"}"
)

libraryDependencies ++= Dependencies.scodec.value
