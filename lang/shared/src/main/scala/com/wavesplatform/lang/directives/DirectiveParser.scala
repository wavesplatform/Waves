package com.wavesplatform.lang.directives

import com.wavesplatform.lang.ExecutionError
import fastparse.WhitespaceApi
import fastparse.core.Parsed.Success
import cats.implicits._

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

  private val directiveKeyP: P[String] =
    P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9') | CharIn("_"))
      .repX(min = 1).!

  private val directiveValueP: P[String] =
    P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9'))
      .repX(min = 1).!

  private val parser: P[Either[ExecutionError, Directive]] =
    P(space ~ start ~ directiveKeyP ~ directiveValueP ~ end ~ space)
      .map {
        case (keyRaw, valueRaw) =>
          for {
            key   <- DirectiveKey.textMap.get(keyRaw).toRight(s"Illegal directive key $keyRaw")
            value <- key match {
              case k: PredefinedDirectiveKey => k.valueDic.textMap.get(valueRaw).toRight(s"Illegal directive value $valueRaw for key $keyRaw")
              case k: ArbitraryDirectiveKey  => Right(k.valueMapper(valueRaw))
            }
          } yield Directive(key, value)
      }

  def apply(input: String): Either[ExecutionError, List[Directive]] =
    input.split("\n")
      .map(parser.parse(_))
      .collect { case Success(value, _) => value }
      .toList
      .sequence
}
