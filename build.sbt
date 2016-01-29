import com.typesafe.config._

val appConf = ConfigFactory.parseFile(new File("src/main/resources/application.conf")).resolve().getConfig("app")

lazy val commonSettings = Seq(
  organization := "org.consensusresearch",
  version := appConf.getString("version"),
  scalaVersion := "2.11.7"
)

def subModule(id: String): Project = Project(id = id, base = file(s"scorex-$id"))

lazy val basics = subModule("basics")
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val transaction = subModule("transaction")
  .aggregate(basics)
  .dependsOn(basics)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val consensus = subModule("consensus")
  .aggregate(basics)
  .dependsOn(basics)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val perma = subModule("perma")
  .aggregate(basics)
  .dependsOn(basics)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

lazy val root = Project(id = "scorex", base = file("."))
  .dependsOn(basics, transaction, consensus, perma)
  .settings(commonSettings: _*)
  .settings(
    testOptions in Test := Seq(Tests.Filter(_.matches(".*TestSuite$")))
  )

name := appConf.getString("product")

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage := Some(url("https://github.com/ConsensusResearch/Scorex-Lagonaki"))

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++=
  Dependencies.db ++
  Dependencies.spray ++
  Dependencies.akka ++
  Dependencies.serizalization ++
  Dependencies.testKit ++
  Dependencies.logging

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//assembly settings
assemblyJarName in assembly := "scorex.jar"

test in assembly := {}

mainClass in assembly := Some("scorex.lagonaki.server.Server")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


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


pomIncludeRepository := { _ => false }

licenses in ThisBuild := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage in ThisBuild := Some(url("https://github.com/ConsensusResearch/Scorex-Lagonaki"))

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:ConsensusResearch/Scorex-Lagonaki.git</url>
    <connection>scm:git:git@github.com:ConsensusResearch/Scorex-Lagonaki.git</connection>
  </scm>
    <developers>
      <developer>
        <id>kushti</id>
        <name>Alexander Chepurnoy</name>
        <url>http://chepurnoy.org/</url>
      </developer>
    </developers>