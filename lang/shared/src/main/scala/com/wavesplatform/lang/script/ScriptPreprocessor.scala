package com.wavesplatform.lang.script

import cats.data.NonEmptyChain
import cats.kernel.CommutativeSemigroup
import cats.implicits._
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.directives.values.{Imports, Library, ScriptType}

object ScriptPreprocessor {
  def apply(
    scriptText: String,
    libraries:  Map[String, String] = Map(),
    ds:         DirectiveSet
  ): Either[String, String] =
    for {
      matchedLibraries <- resolveLibraries(libraries, ds.imports)
      _                <- checkLibrariesDirectives(matchedLibraries, ds.scriptType)
    } yield gatherScriptText(scriptText, matchedLibraries)

  private def resolveLibraries(
    libraries: Map[String, String],
    imports:   Imports
  ): Either[String, Map[String, String]] = {
    implicit val cs: CommutativeSemigroup[NonEmptyChain[String]] = _ ++ _
    imports.fileNames
      .map(f => (f, (f, libraries.get(f))))
      .toMap
      .unorderedTraverse { case (name, expr) => expr.toValidNec(name) }
      .leftMap(f => s"Unresolved imports: ${f.map(s => s"`$s`").toList.mkString(", ")}")
      .toEither
  }

  private def checkLibrariesDirectives(
    matchedLibraries: Map[String, String],
    scriptType:       ScriptType
  ): Either[String, List[Unit]] =
    matchedLibraries
      .map { case (name, src) => checkLibraryDirectives(scriptType, name, src) }
      .toList
      .sequence

  private def checkLibraryDirectives(
    scriptType:  ScriptType,
    libraryName: String,
    librarySrc:  String
  ): Either[String, Unit] =
    for {
      directives <- DirectiveParser(librarySrc)
      ds         <- Directive.extractDirectives(directives)
      _          <- Either.cond(ds.contentType == Library,   (), s"CONTENT_TYPE of `$libraryName` is not LIBRARY")
      _          <- Either.cond(ds.scriptType == scriptType, (), s"SCRIPT_TYPE of `$libraryName` is $scriptType should be the same with script")
    } yield ()

  private val importRegex = s"\\${DirectiveParser.start}\\s*${DirectiveKey.IMPORT.text}.*${DirectiveParser.end}"
  private val directiveRegex = s"\\${DirectiveParser.start}.*${DirectiveParser.end}"

  private def gatherScriptText(src: String, libraries: Map[String, String]) = {
    val additionalDecls = libraries.values.map(removeDirectives).mkString("\n")
    src.replaceFirst(importRegex, additionalDecls)
  }

  private def removeDirectives(script: String): String =
    script.replaceAll(directiveRegex, "")
}
