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
import sbt.internal.BuildDependencies
import sbtcompat.PluginCompat.{FileRef, parseArtifactStrAttribute, parseModuleIDStrAttribute, toFile, toFileRef}

/**
  * @note Specify "maintainer" to solve DEB warnings
  */
object ExtensionPackaging extends AutoPlugin {

  object autoImport extends ExtensionKeys
  import autoImport._

  override def requires: Plugins = UniversalDeployPlugin && CommonSettings && JDebPackaging

  override def projectSettings: Seq[Def.Setting[?]] =
    Seq(
      packageDoc / publishArtifact := false,
      packageSrc / publishArtifact := false,
      // Here we record the classpath as it's added to the mappings separately, so
      // we can use its order to generate the bash/bat scripts.
      // Uncached: sbt 2 has no JsonFormat for FileRef under its sbt2-compat alias.
      classpathOrdering := Def.uncached(Nil),
      // Note: This is sometimes on the classpath via dependencyClasspath in Runtime.
      // We need to figure out why sometimes the Attributed[FileRef] is correctly configured
      // and sometimes not.
      classpathOrdering += Def.uncached {
        val jar = (Compile / packageBin).value
        val id  = projectID.value
        val art = (Compile / packageBin / artifact).value
        jar -> ("lib/" + makeJarName(id.organization, id.name, id.revision, art.name, art.classifier))
      },
      classpathOrdering ++= Def.uncached {
        implicit val conv: xsbti.FileConverter = fileConverter.value
        excludeProvidedArtifacts((Runtime / dependencyClasspath).value, findProvidedArtifacts.value)
      },
      extensionClasses := Nil,
      Universal / mappings ++= {
        implicit val conv: xsbti.FileConverter = fileConverter.value
        classpathOrdering.value ++ {
          val baseConfigName = s"${name.value}-${network.value}.conf"
          val localFile      = (Compile / baseDirectory).value / baseConfigName
          if (localFile.exists()) {
            val artifactPath = s"doc/${name.value}.conf.sample"
            Seq(toFileRef(localFile) -> artifactPath)
          } else Seq.empty
        }
      },
      classpath := Def.uncached(makeRelativeClasspathNames(classpathOrdering.value)),
      nodePackageName := (LocalProject("node") / Linux / packageName).value,
      debianPackageDependencies +=
        s"${(LocalProject("node") / Debian / packageName).value} (= ${(LocalProject("node") / version).value})",
      // To write files to Waves NODE directory
      linuxPackageMappings := {
        implicit val conv: xsbti.FileConverter = fileConverter.value
        getUniversalFolderMappings(
          nodePackageName.value,
          defaultLinuxInstallLocation.value,
          (Universal / mappings).value
        )
      },
      Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postinst)(
        Postinst ->
          s"""#!/bin/sh
             |set -e
             |chown -R ${nodePackageName.value}:${nodePackageName.value} /usr/share/${nodePackageName.value}""".stripMargin
      ),
      Linux / maintainer := "wavesplatform.com",
      Linux / packageSummary := s"Waves node ${name.value}${network.value.packageSuffix} extension",
      Linux / packageDescription := s"Waves node ${name.value}${network.value.packageSuffix} extension",
      Debian / normalizedName := s"${name.value}${network.value.packageSuffix}",
      Debian / packageName := s"${name.value}${network.value.packageSuffix}",
      libraryDependencies ++= Dependencies.logDeps,
      run / javaOptions ++= extensionClasses.value.zipWithIndex.map { case (extension, index) => s"-Dwaves.extensions.$index=$extension" }
    )

  // A copy of com.typesafe.sbt.packager.linux.LinuxPlugin.getUniversalFolderMappings
  private def getUniversalFolderMappings(pkg: String, installLocation: String, mappings: Seq[(FileRef, String)])(implicit
      conv: xsbti.FileConverter
  ): Seq[LinuxPackageMapping] = {
    def isWindowsFile(f: (FileRef, String)): Boolean = f._2.endsWith(".bat")

    val filtered = mappings.filterNot(isWindowsFile).map { case (ref, path) => toFile(ref) -> path }
    if (filtered.isEmpty) Seq.empty
    else mapGenericMappingsToLinux(filtered, Users.Root, Users.Root)(name => installLocation + "/" + pkg + "/" + name)
  }

  private def makeRelativeClasspathNames(mappings: Seq[(FileRef, String)]): Seq[String] =
    for {
      (_, name) <- mappings
    } yield {
      // Here we want the name relative to the lib/ folder...
      // For now we just cheat...
      if (name.startsWith("lib/")) name.drop(4)
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
  private def getJarFullFilename(dep: Attributed[FileRef]): String = {
    val filename: Option[String] = for {
      module   <- dep.metadata.get(moduleIDStr).map(parseModuleIDStrAttribute)
      artifact <- dep.metadata.get(artifactStr).map(parseArtifactStrAttribute)
    } yield makeJarName(module.organization, module.name, module.revision, artifact.name, artifact.classifier)
    filename.getOrElse(dep.data.name)
  }

  // Here we grab the dependencies...
  private def dependencyProjectRefs(build: BuildDependencies, thisProject: ProjectRef): Seq[ProjectRef] =
    build.classpathTransitive.getOrElse(thisProject, Nil)

  private def isRuntimeArtifact(dep: Attributed[FileRef]): Boolean =
    dep.get(artifactStr).map(parseArtifactStrAttribute).map(a => a.`type` == "jar" || a.`type` == "bundle").getOrElse {
      val name = dep.data.name
      !(name.endsWith(".jar") || name.endsWith("-sources.jar") || name.endsWith("-javadoc.jar"))
    }

  private def findProvidedArtifacts: Def.Initialize[Task[Classpath]] =
    Def.taskDyn {
      val refs   = dependencyProjectRefs(buildDependencies.value, thisProjectRef.value)
      val filter = ScopeFilter(inProjects(refs*))
      Def.task {
        (Runtime / dependencyClasspath).all(filter).value.flatten.filter(isRuntimeArtifact).distinct
      }
    }

  private def excludeProvidedArtifacts(runtimeClasspath: Classpath, exclusions: Classpath)(implicit
      conv: xsbti.FileConverter
  ): Seq[(FileRef, String)] = {
    val excludedArtifacts = (for {
      a <- exclusions
      moduleID = a.get(moduleIDStr).map(parseModuleIDStrAttribute)
    } yield (moduleID.map(_.organization), moduleID.map(_.name))).toSet

    (for {
      r <- runtimeClasspath
      if toFile(r.data).isFile
      moduleID = r.get(moduleIDStr).map(parseModuleIDStrAttribute)
      if !excludedArtifacts((moduleID.map(_.organization), moduleID.map(_.name)))
    } yield r.data -> ("lib/" + getJarFullFilename(r))).distinct
  }
}
