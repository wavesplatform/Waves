import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.{Serde, TypeChecker}
import fastparse.core.Parsed
import scodec.bits.BitVector

import scala.scalajs.js.annotation.JSExportTopLevel

object JsAPI {
  @JSExportTopLevel("parse")
  def parse(input: String): Parsed[Terms.Expressions.EXPR, Char, String] =
    Parser(input)

  @JSExportTopLevel("compile")
  def compile(input: String): Option[BitVector] = {
    parse(input)
      .fold[Option[Terms.Expressions.EXPR]]((_, _, _) => None, (x, _) => Some(x))
      .flatMap(TypeChecker(TypeChecker.TypeCheckerContext.empty, _).toOption)
      .flatMap(Serde.codec.encode(_).toOption)
  }
}
