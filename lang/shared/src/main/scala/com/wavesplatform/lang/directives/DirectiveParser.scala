package com.wavesplatform.lang.directives

import fastparse.WhitespaceApi
import fastparse.core.Parsed.Success

object DirectiveParser {

  private val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharIn(" ", "\t", "\r", "\n").rep)
  }

  import White._
  import fastparse.noApi._

  private val space: P[Unit] =
    P( CharIn(" ", "\t", "\r", "\n").rep )

  private val directiveKeyP: P[DirectiveKey] =
    P( StringIn(DirectiveKey.dictionary.keys.toSeq: _*).! )
      .map(DirectiveKey.dictionary)

  private val directiveValueP: P[String] =
    P( CharIn('a' to 'z') | CharIn('A' to 'Z') | CharIn('0' to '9') )
      .repX(min = 1).!

  private val parser: P[Directive] =
    P(space ~ "{-#" ~ directiveKeyP ~ directiveValueP ~ "#-}" ~ space)
      .map({ case (k, v) => Directive(k, v) })

  def apply(input: String): List[Directive] = {
    input.split("\n")
      .map(str => parser.parse(str))
      .collect({ case Success(value, _) => value })
      .toList
  }
}
