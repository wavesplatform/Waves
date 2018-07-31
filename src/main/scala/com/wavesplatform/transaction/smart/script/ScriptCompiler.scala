package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.utils
import com.wavesplatform.utils.functionCosts
import com.wavesplatform.transaction.smart.script.v1.ScriptV1

import scala.util.{Failure, Success, Try}

object ScriptCompiler {

  private val v1Compiler = new CompilerV1(utils.dummyCompilerContext)

  def apply(scriptText: String): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      v <- extractVersion(directives)
      expr <- v match {
        case V1 => v1Compiler.compile(scriptWithoutDirectives, directives)
      }
      script     <- ScriptV1(expr)
      complexity <- ScriptEstimator(functionCosts, expr)
    } yield (script, complexity)
  }

  def estimate(script: Script): Either[String, Long] = script match {
    case Script.Expr(expr) => ScriptEstimator(functionCosts, expr)
  }

  private def extractVersion(directives: List[Directive]): Either[String, ScriptVersion] = {
    directives
      .find(_.key == DirectiveKey.LANGUAGE_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            ScriptVersion
              .fromInt(v)
              .fold[Either[String, ScriptVersion]](Left("Unsupported language version"))(_.asRight)
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(V1.asRight)
  }

}
