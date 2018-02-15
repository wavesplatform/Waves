import com.wavesplatform.lang.{Parser, Terms}
import fastparse.core.Parsed

import scala.scalajs.js.annotation.JSExportTopLevel

object JsAPI {
  @JSExportTopLevel("parse")
  def parse(input: String): Parsed[Terms.Expr, Char, String] =
    Parser(input)
}
