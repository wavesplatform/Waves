import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.{Promise, UndefOr}
import scala.scalajs.js.Dynamic.literal as jObj
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.repl.node.http.WebEnvironment.executionContext

object JsAPI {
  @JSExportTopLevel("repl")
  def repl(
      settings: UndefOr[NodeConnectionSettings],
      libraries: js.Array[String] = js.Array()
  ): js.Dynamic = asJs(Repl(settings.toOption, libraries.toList))

  private def asJs(repl: Repl): js.Dynamic =
    jObj(
      "evaluate"    -> (repl.execute _ andThen mapResult),
      "info"        -> repl.info _,
      "totalInfo"   -> (() => repl.totalInfo),
      "clear"       -> repl.clear _,
      "reconfigure" -> (repl.reconfigure _ andThen asJs)
    )

  private def mapResult(eval: Future[Either[String, String]]): Promise[js.Object & js.Dynamic] =
    eval
      .map(
        _.fold(
          e => jObj("error" -> e),
          r => jObj("result" -> r)
        )
      )
      .toJSPromise
}
