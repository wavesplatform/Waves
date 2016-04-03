import java.io.File

lazy val JarPath = "target/scala-2.11/waves.jar"

val recompile = taskKey[File]("Recompile project")
val start = taskKey[Unit]("Connect to the network")
val startLocal1 = taskKey[Unit]("Run local peer with settings-local1.json")
val startLocal2 = taskKey[Unit]("Run local peer with settings-local2.json")
val startLocal3 = taskKey[Unit]("Run local peer with settings-local3.json")
val startLocal = taskKey[Unit]("Run both peers")

recompile := {
  clean.value
  assembly.value
}

start := {
  val jarPath: String = recompile.value.getPath
  runPeer(jarPath, "settings.json")
}

startLocal1 := {
  runPeer(JarPath, "settings-local1.json")
}

startLocal2 := {
  runPeer(JarPath, "settings-local2.json")
}

startLocal3 := {
  runPeer(JarPath, "settings-local3.json")
}

startLocal := {
  val jarPath: String = recompile.value.getPath
  runPeer(jarPath, "settings-local1.json", "&")
  runPeer(jarPath, "settings-local2.json")
}

def runPeer(jarPath: String, configName: String, params: String*): Unit = {
  val config = new File(configName)
  s"java -jar $jarPath ${config.getAbsolutePath} ${params.mkString(" ")}".!
}
