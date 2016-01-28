import java.io.File

lazy val JarPath = "target/scala-2.11/scorex.jar"

val recompile = taskKey[File]("Recompile project")
val start = taskKey[Unit]("Connect to the network")
val pack = taskKey[Unit]("Create packages to usual systems")

recompile := {
  clean.value
  assembly.value
}

start := {
  val jarPath: String = recompile.value.getPath
  runPeer(jarPath, "settings.json")
}

def runPeer(jarPath: String, configName: String, params: String*): Unit = {
  val config = new File(configName)
  s"java -jar $jarPath ${config.getAbsolutePath} ${params.mkString(" ")}".!
}
