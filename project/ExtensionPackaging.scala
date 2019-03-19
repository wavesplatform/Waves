import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.packager.Compat._
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging.autoImport.scriptClasspath
import com.typesafe.sbt.packager.archetypes.JavaServerAppPackaging
import com.typesafe.sbt.packager.universal.UniversalDeployPlugin
import sbt.Keys._
import sbt._

object ExtensionPackaging extends AutoPlugin {

  object autoImport extends ExtensionKeys
  import autoImport._

  override def requires: Plugins = UniversalDeployPlugin && JavaServerAppPackaging

  override def projectSettings: Seq[Def.Setting[_]] = Defaults.itSettings ++ Seq(
    publishArtifact in packageDoc := false,
    publishArtifact in packageSrc := false,
    javaOptions in Universal := Nil,
    // Here we record the classpath as it's added to the mappings separately, so
    // we can use its order to generate the bash/bat scripts.
    classpathOrdering := Nil,
    // Note: This is sometimes on the classpath via dependencyClasspath in Runtime.
    // We need to figure out why sometimes the Attributed[File] is correctly configured
    // and sometimes not.
    classpathOrdering += {
      val jar = (packageBin in Compile).value
      val id  = projectID.value
      val art = (artifact in Compile in packageBin).value
      jar -> ("lib/" + makeJarName(id.organization, id.name, id.revision, art.name, art.classifier))
    },
    classpathOrdering ++= excludeProvidedArtifacts((dependencyClasspath in Runtime).value, findProvidedArtifacts.value),
    mappings in Universal ++= classpathOrdering.value,
    classpath := makeRelativeClasspathNames(classpathOrdering.value),
    scriptClasspath += "*"
  )

  private def makeRelativeClasspathNames(mappings: Seq[(File, String)]): Seq[String] =
    for {
      (_, name) <- mappings
    } yield {
      // Here we want the name relative to the lib/ folder...
      // For now we just cheat...
      if (name startsWith "lib/") name drop 4
      else "../" + name
    }

  /**
    * Constructs a jar name from components...(ModuleID/Artifact)
    */
  def makeJarName(org: String, name: String, revision: String, artifactName: String, artifactClassifier: Option[String]): String =
    org + "." +
      name + "-" +
      Option(artifactName.replace(name, "")).filterNot(_.isEmpty).map(_ + "-").getOrElse("") +
      revision +
      artifactClassifier.filterNot(_.isEmpty).map("-" + _).getOrElse("") +
      ".jar"

  // Determines a nicer filename for an attributed jar file, using the
  // ivy metadata if available.
  private def getJarFullFilename(dep: Attributed[File]): String = {
    val filename: Option[String] = for {
      module <- dep.metadata
      // sbt 0.13.x key
        .get(AttributeKey[ModuleID]("module-id"))
        // sbt 1.x key
        .orElse(dep.metadata.get(AttributeKey[ModuleID]("moduleID")))
      artifact <- dep.metadata.get(AttributeKey[Artifact]("artifact"))
    } yield makeJarName(module.organization, module.name, module.revision, artifact.name, artifact.classifier)
    filename.getOrElse(dep.data.getName)
  }

  // Here we grab the dependencies...
  private def dependencyProjectRefs(build: BuildDependencies, thisProject: ProjectRef): Seq[ProjectRef] =
    build.classpathTransitive.getOrElse(thisProject, Nil)

  private def isRuntimeArtifact(dep: Attributed[File]): Boolean =
    dep.get(sbt.Keys.artifact.key).map(a => a.`type` == "jar" || a.`type` == "bundle").getOrElse {
      val name = dep.data.getName
      !(name.endsWith(".jar") || name.endsWith("-sources.jar") || name.endsWith("-javadoc.jar"))
    }

  private def findProvidedArtifacts: Def.Initialize[Task[Classpath]] =
    Def
      .task {
        val stateTask = state.taskValue
        val refs      = dependencyProjectRefs(buildDependencies.value, thisProjectRef.value)

        val providedClasspath = refs.map { ref =>
          stateTask.flatMap { state =>
            val extracted = Project.extract(state)
            extracted.get(Runtime / dependencyClasspath in ref)
          }
        }

        val allProvidedClasspath: Task[Classpath] =
          providedClasspath
            .fold[Task[Classpath]](task(Nil)) { (prev, next) =>
              for {
                p <- prev
                n <- next
              } yield p ++ n.filter(isRuntimeArtifact)
            }
            .map(_.distinct)

        allProvidedClasspath
      }
      .flatMap(identity)

  private def excludeProvidedArtifacts(runtimeClasspath: Classpath, exclusions: Classpath): Seq[(File, String)] = {
    val excludedArtifacts = (for {
      a <- exclusions
      moduleID = a.get(Keys.moduleID.key)
    } yield (moduleID.map(_.organization), moduleID.map(_.name))).toSet

    (for {
      r <- runtimeClasspath
      if r.data.isFile
      moduleID = r.get(Keys.moduleID.key)
      if !excludedArtifacts((moduleID.map(_.organization), moduleID.map(_.name)))
    } yield r.data -> ("lib/" + getJarFullFilename(r))).distinct
  }
}
