import com.wavesplatform.lang.{Parser, Serde, Terms, TypeChecker}
import fastparse.core.Parsed
import scodec.bits.BitVector

import scala.scalajs.js.annotation.JSExportTopLevel

object JsAPI {
  @JSExportTopLevel("parse")
  def parse(input: String): Parsed[Terms.Untyped.EXPR, Char, String] =
    Parser(input)

  @JSExportTopLevel("compile")
  def compile(input: String): Option[BitVector] = {
    parse(input).fold[Option[Terms.Untyped.EXPR]]((_, _, _) => None, (x, _) => Some(x))
      .flatMap(TypeChecker(TypeChecker.TypeCheckerContext.empty, _).toOption)
      .flatMap(Serde.codec.encode(_).toOption)
  }
}
