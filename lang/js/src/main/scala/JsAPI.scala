import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import fastparse.core.Parsed
import scodec.bits.BitVector

import scala.scalajs.js.annotation.JSExportTopLevel

object JsAPI {
  @JSExportTopLevel("parse")
  def parse(input: String): Parsed[Expressions.EXPR, Char, String] =
    Parser(input)

  @JSExportTopLevel("compile")
  def compile(input: String): Option[BitVector] = {
    parse(input)
      .fold[Option[Expressions.EXPR]]((_, _, _) => None, (x, _) => Some(x))
      .flatMap(CompilerV1(CompilerContext.empty, _).toOption)
      .flatMap(Serde.codec.encode(_).toOption)
  }
}
