import sbt.Keys.Classpath
import sbt._

trait ExtensionKeys {
  val classpathOrdering = taskKey[Seq[(File, String)]](
    "The order of the classpath used at runtime for the bat/bash scripts."
  )
  val projectDependencyArtifacts = taskKey[Classpath](
    "The set of exported artifacts from our dependent projects."
  )
  val providedArtifacts = taskKey[Classpath](
    "The set of exported artifacts from our dependent projects."
  )

  val classpath = taskKey[Seq[String]](
    "A list of relative filenames (to the lib/ folder in the distribution) of what to include on the classpath."
  )

  val nodePackageName = settingKey[String]("Node deb package name")
}
