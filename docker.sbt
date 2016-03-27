/*
// dockerize

enablePlugins(DockerPlugin)

// Make the docker task depend on the assembly task,
// which generates a fat JAR file
docker <<= docker.dependsOn(sbt.Keys.`package`.in(Compile, packageBin))


dockerfile in docker := {
  val jarFile = (assemblyOutputPath in assembly).value
  val jarTargetPath = s"/app/${jarFile.name}"
  val settingsPath = (baseDirectory in ThisBuild).value / "settings.json"

  new Dockerfile {
    from("frolvlad/alpine-oraclejdk8")
    // runRaw("apk --update add openjdk7-jre")
    // copy compiled jar into the container
    run("mkdir", "-p", "/app")
    copy(jarFile, jarTargetPath)
    // copy settings
    copy(settingsPath, "/app/settings.json")
    // persist data beyond the lifetime of a container session
    run("mkdir", "-p", "/tmp/scorex")
    volume("/tmp/scorex")
    // run scorex as:
    // /usr/bin/java -jar scorex.jar
    workDir("/app")
    run("/usr/bin/java", "-jar", jarTargetPath)
  }
}

// todo: name image
imageNames in docker := Seq(
  ImageName(s"org.consensusresearch/starter")
)

// todo: can drop cache
// buildOptions in docker := BuildOptions(cache = false)
*/