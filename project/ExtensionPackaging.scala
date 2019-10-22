import CommonSettings.autoImport.network
import com.typesafe.sbt.SbtNativePackager.Universal
import com.typesafe.sbt.SbtNativePackager.autoImport.{maintainer, packageDescription, packageSummary}
import com.typesafe.sbt.packager.Compat._
import com.typesafe.sbt.packager.Keys.{debianPackageDependencies, maintainerScripts, packageName}
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging.autoImport.maintainerScriptsAppend
import com.typesafe.sbt.packager.debian.DebianPlugin.Names.Postinst
import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.Debian
import com.typesafe.sbt.packager.debian.JDebPackaging
import com.typesafe.sbt.packager.linux.LinuxPackageMapping
import com.typesafe.sbt.packager.linux.LinuxPlugin.autoImport.{Linux, defaultLinuxInstallLocation, linuxPackageMappings}
import com.typesafe.sbt.packager.linux.LinuxPlugin.{Users, mapGenericMappingsToLinux}
import com.typesafe.sbt.packager.universal.UniversalDeployPlugin
import sbt.Keys._
import sbt._

/**
  * @note Specify "maintainer" to solve DEB warnings
  */
object ExtensionPackaging extends AutoPlugin {

  object autoImport extends ExtensionKeys
  import autoImport._

  override def requires: Plugins = UniversalDeployPlugin && CommonSettings && JDebPackaging

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      packageName := s"${name.value}${network.value.packageSuffix}",
      packageDoc / publishArtifact := false,
      packageSrc / publishArtifact := false,
      Universal / javaOptions := Nil,
      // Here we record the classpath as it's added to the mappings separately, so
      // we can use its order to generate the bash/bat scripts.
      classpathOrdering := Nil,
      // Note: This is sometimes on the classpath via dependencyClasspath in Runtime.
      // We need to figure out why sometimes the Attributed[File] is correctly configured
      // and sometimes not.
      classpathOrdering += {
        val jar = (Compile / packageBin).value
        val id  = projectID.value
        val art = (Compile / packageBin / artifact).value
        jar -> ("lib/" + makeJarName(id.organization, id.name, id.revision, art.name, art.classifier))
      },
      classpathOrdering ++= excludeProvidedArtifacts((Runtime / dependencyClasspath).value, findProvidedArtifacts.value),
      Universal / mappings ++= classpathOrdering.value ++ {
        val baseConfigName = s"${name.value}-${network.value}.conf"
        val localFile      = (Compile / baseDirectory).value / baseConfigName
        if (localFile.exists()) {
          val artifactPath = s"doc/${name.value}.conf.sample"
          Seq(localFile -> artifactPath)
        } else Seq.empty
      },
      classpath := makeRelativeClasspathNames(classpathOrdering.value),
      nodePackageName := (LocalProject("node") / Linux / packageName).value,
      debianPackageDependencies := Seq((LocalProject("node") / Debian / packageName).value),
      // To write files to Waves NODE directory
      linuxPackageMappings := getUniversalFolderMappings(
        nodePackageName.value,
        defaultLinuxInstallLocation.value,
        (Universal / mappings).value
      ),
      Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postinst)(
        Postinst ->
          s"""#!/bin/sh
             |set -e
             |chown -R ${nodePackageName.value}:${nodePackageName.value} /usr/share/${nodePackageName.value}""".stripMargin
      ),
      libraryDependencies ++= Dependencies.logDeps
    ) ++ nameFix ++ inScope(Global)(nameFix) ++ maintainerFix

  private def maintainerFix = inConfig(Linux)(
    Seq(
      maintainer := "wavesplatform.com",
      packageSummary := s"Waves node ${name.value}${network.value.packageSuffix} extension",
      packageDescription := s"Waves node ${name.value}${network.value.packageSuffix} extension"
    ))
  
  private def nameFix = Seq(
    packageName := s"${name.value}${network.value.packageSuffix}",
    normalizedName := s"${name.value}${network.value.packageSuffix}"
  )

  // A copy of com.typesafe.sbt.packager.linux.LinuxPlugin.getUniversalFolderMappings
  private def getUniversalFolderMappings(pkg: String, installLocation: String, mappings: Seq[(File, String)]): Seq[LinuxPackageMapping] = {
    def isWindowsFile(f: (File, String)): Boolean = f._2 endsWith ".bat"

    val filtered = mappings.filterNot(isWindowsFile)
    if (filtered.isEmpty) Seq.empty
    else mapGenericMappingsToLinux(filtered, Users.Root, Users.Root)(name => installLocation + "/" + pkg + "/" + name)
  }

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
