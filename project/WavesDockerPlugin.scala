import java.io.File

import WavesDockerKeys._
import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Def, Plugins, inTask, taskKey}
import sbtdocker.DockerPlugin
import sbtdocker.DockerPlugin.autoImport._

object WavesDockerPlugin extends AutoPlugin {
  override def requires: Plugins = JvmPlugin && DockerPlugin
  
  override def projectSettings: Seq[Def.Setting[_]] =
    inTask(docker)(
      Seq(
        additionalFiles := Seq.empty,
        exposedPorts := Set.empty,
        baseImage := "anapsix/alpine-java:8_server-jre",
        dockerfile := {
          val yourKitArchive = "YourKit-JavaProfiler-2019.8-docker.zip"
          val bin            = "/opt/waves/start-waves.sh"

          new Dockerfile {
            from(baseImage.value)

            runRaw(s"""mkdir -p /opt/waves && \\
                    |apk update && \\
                    |apk add --no-cache openssl ca-certificates && \\
                    |wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O /opt/waves/aspectjweaver.jar && \\
                    |wget --quiet "https://www.yourkit.com/download/docker/$yourKitArchive" -P /tmp/ && \\
                    |unzip /tmp/$yourKitArchive -d /usr/local && \\
                    |rm -f /tmp/$yourKitArchive""".stripMargin)

            add(additionalFiles.value, "/opt/waves/")
            runShell("chmod", "+x", bin)
            entryPoint(bin)
            expose(exposedPorts.value.toSeq: _*)
          }
        },
        buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
      ))
}

object WavesDockerKeys {
  val additionalFiles = taskKey[Seq[File]]("Additional files to copy to /opt/waves")
  val exposedPorts    = taskKey[Set[Int]]("Exposed ports")
  val baseImage       = taskKey[String]("A base image for this container")
}
