import sbt.Keys.Classpath
import sbt._

trait ExtensionKeys {
  val classpathOrdering = TaskKey[Seq[(File, String)]](
    "classpathOrdering",
    "The order of the classpath used at runtime for the bat/bash scripts."
  )
  val projectDependencyArtifacts = TaskKey[Classpath](
    "projectDependencyArtifacts",
    "The set of exported artifacts from our dependent projects."
  )
  val providedArtifacts = TaskKey[Classpath](
    "providedArtifacts",
    "The set of exported artifacts from our dependent projects."
  )
  
  val classpath = TaskKey[Seq[String]](
    "classpath",
    "A list of relative filenames (to the lib/ folder in the distribution) of what to include on the classpath."
  )
}
