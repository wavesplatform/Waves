package com.wavesplatform.lang.directives

import com.wavesplatform.lang.ExecutionError
import fastparse.WhitespaceApi
import fastparse.core.Parsed.{Failure, Success}
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
    P(CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9') | CharIn(Seq('/', '\\', '.', ',')))
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
      .filter(_.matches(s"\\s*\\$start.*$end\\s*"))
      .map(parser.parse(_))
      .foldLeft(Map[DirectiveKey, Directive]().asRight[ExecutionError]) {
        case (err: Left[_, _], _)                                      => err
        case (_, _: Failure[_, _])                                     => Left(s"Directive $input has illegal format")
        case (_, Success(Left(err), _))                                => Left(err)
        case (Right(acc), Success(Right(d), _)) if acc.contains(d.key) => Left(s"Directive key ${d.key.text} is used more than once")
        case (Right(acc), Success(Right(d), _))                        => Right(acc + (d.key -> d))
      }
      .map(_.values.toList)

  case class ParseResult(keys: Set[DirectiveKey], directives: List[Directive])
}
