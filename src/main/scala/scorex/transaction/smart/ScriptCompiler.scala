package scorex.transaction.smart

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.directives.{Directive, DirectiveKey, DirectiveParser}
import com.wavesplatform.lang.v1.{CompilerV1, ScriptExprV1}
import com.wavesplatform.lang.{ScriptExpr, ScriptVersion}
import com.wavesplatform.utils

import scala.util.Try

object ScriptCompiler {

  private val v1Compiler = new CompilerV1(utils.dummyTypeCheckerContext)

  def apply(scriptText: String): Either[String, ScriptExpr] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.lines
        .filter(str => str.contains("{-#"))
        .mkString("\n")

    extractVersion(directives)
      .flatMap(v =>
        v match {
          case V1 =>
            v1Compiler
              .compile(scriptWithoutDirectives, directives)
              .map(ScriptExprV1)
      })
  }

  private def extractVersion(directives: List[Directive]): Either[String, ScriptVersion] = {
    directives
      .find(_.key == DirectiveKey.LANGUAGE_VERSION)
      .flatMap(d => Try(d.value.toInt).toOption)
      .flatMap(ScriptVersion.fromInt)
      .fold[Either[String, ScriptVersion]](Left("Script version unsupported or not specified"))(_.asRight)
  }
}
