package com.wavesplatform.lang.directives

import com.wavesplatform.lang.ExecutionError
import fastparse.WhitespaceApi
import fastparse.core.Parsed.{Failure, Success}
import cats.implicits._
import com.wavesplatform.lang.directives.values.DirectiveValue

object DirectiveParser {

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import White._
  import fastparse.noApi._

  val start = "{-#"
  val end   = "#-}"

  private val space: P[Unit] =
    P(CharIn(" ", "\t", "\r", "\n").rep)

  private val directiveKeyP: P[DirectiveKey] =
    P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9') | CharIn("_"))
      .repX(min = 1)
      .!
      .map(DirectiveKey.textMap)

  private val directiveValueP: P[String] =
    P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9'))
      .repX(min = 1).!

  private val parser: P[Directive] =
    P(space ~ start ~ directiveKeyP ~ directiveValueP ~ end ~ space)
      .map {
        case (key, value) => Directive(key, key.valueDic.textMap(value))
      }

  def apply(input: String): Either[ExecutionError, List[Directive]] = {
    input
      .split("\n")
      .filter(_.trim.nonEmpty)
      .toList
      .traverse(parser.parse(_) match {
        case Success(value, _) => Right(value)
        case Failure(_, _, _)  => Left(s"Directive parse error for $input")
      })
  }
}
