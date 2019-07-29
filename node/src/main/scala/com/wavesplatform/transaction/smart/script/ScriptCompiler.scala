package com.wavesplatform.transaction.smart.script

import cats.data.NonEmptyChain
import cats.implicits._
import cats.kernel.CommutativeSemigroup
import com.wavesplatform.lang.directives._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ContractCompiler, ExpressionCompiler}
import com.wavesplatform.utils._

import scala.util.Try

object ScriptCompiler extends ScorexLogging {

  @Deprecated
  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] =
    for {
      directives <- DirectiveParser(scriptText)
      scriptType  = if (isAssetScript) Asset else Account
      script     <- compileWithDirectives(scriptText, Directive(scriptType.key, scriptType) :: directives)
    } yield script

  def compile(
    scriptText: String,
    libraries:  Map[String, String] = Map()
  ): Either[String, (Script, Long)] =
    for {
      directives <- DirectiveParser(scriptText)
      result     <- compileWithDirectives(scriptText, directives, libraries)
    } yield result

  private def compileWithDirectives(
    scriptText: String,
    directives: List[Directive],
    libraries:  Map[String, String] = Map()
  ): Either[String, (Script, Long)] =
    for {
      ds     <- Directive.extractDirectives(directives)
      script <- tryCompile(scriptText, ds, libraries)
    } yield (script, script.complexity)

  private def tryCompile(
    scriptText: String,
    ds:         DirectiveSet,
    libraries:  Map[String, String]
  ): Either[String, Script] = {
    val ctx = compilerContext(ds)
    val src2Script = selectScriptCreation(ds)
    for {
      additionalDecls <- resolveLibraries(libraries, ds.imports)
      gatheredScriptText = additionalDecls.foldLeft(removeDirectives(scriptText)) { case (acc, decl) =>
        removeDirectives(decl) + "\n" + acc
      }
      //todo check libraries directives
      r <- Try(src2Script(gatheredScriptText, ctx))
        .toEither
        .leftMap { ex =>
          log.error("Error compiling script", ex)
          log.error(scriptText)
          Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        }
        .flatten
    } yield r
  }

  private def selectScriptCreation(ds: DirectiveSet): (String, CompilerContext) => Either[String, Script] =
    ds.contentType match {
      case Expression => ExpressionCompiler.compile(_, _).flatMap(ExprScript(ds.stdLibVersion, _))
      case DApp       => ContractCompiler.compile(_, _).flatMap(ContractScript(ds.stdLibVersion, _))
      case Library    => ExpressionCompiler.compileDecls(_, _).flatMap(ExprScript(ds.stdLibVersion, _))
    }

  private def removeDirectives(script: String): String = {
    script.replaceAll("\\{-#.*#-}", "")
  }

  private def resolveLibraries(
    libraries: Map[String, String],
    imports: Imports
  ): Either[String, List[String]] = {
    implicit val cs: CommutativeSemigroup[NonEmptyChain[String]] = _ ++ _
    imports.fileNames
      .map(f => (f, (f, libraries.get(f))))
      .toMap
      .unorderedTraverse { case (name, expr) => expr.toValidNec(name) }
      .leftMap(f => s"Unresolved imports: ${f.map(s => s"`$s`").toList.mkString(", ")}")
      .map(_.values.toList)
      .toEither
  }

  def estimate(script: Script, version: StdLibVersion): Either[String, Long] = script match {
    case s: ExprScript         => ScriptEstimator(varNames(version, Expression), functionCosts(version), s.expr)
    case s: ContractScriptImpl => ContractScript.estimateComplexity(version, s.expr).map(_._1)
    case _                     => ???
  }
}
