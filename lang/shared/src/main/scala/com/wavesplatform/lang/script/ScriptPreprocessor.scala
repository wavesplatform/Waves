package com.wavesplatform.lang.script

import cats.data.NonEmptyChain
import cats.kernel.CommutativeSemigroup
import cats.implicits._
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesplatform.lang.directives.values.{Imports, Library}

object ScriptPreprocessor {
  def apply(
    scriptText: String,
    libraries:  Map[String, String],
    imports:    Imports
  ): Either[String, String] =
    for {
      matchedLibraries <- resolveLibraries(libraries, imports)
      _                <- checkLibrariesDirectives(matchedLibraries)
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
    matchedLibraries: Map[String, String]
  ): Either[String, List[Unit]] =
    matchedLibraries
      .map { case (name, src) => checkLibraryDirectives(name, src) }
      .toList
      .sequence

  private def checkLibraryDirectives(
    libraryName: String,
    librarySrc:  String
  ): Either[String, Unit] =
    for {
      directives <- DirectiveParser(librarySrc)
      ds         <- Directive.extractDirectives(directives)
      _          <- Either.cond(ds.contentType == Library, (), s"CONTENT_TYPE of `$libraryName` is not LIBRARY")
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
