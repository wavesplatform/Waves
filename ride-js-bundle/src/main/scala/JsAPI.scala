import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Promise
import scala.scalajs.js.Dynamic.literal as jObj
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.repl.node.http.WebEnvironment.executionContext

object JsAPI {
  @JSExportTopLevel("repl", moduleID = "ride")
  def repl(
      settings: js.UndefOr[js.Dynamic] = js.undefined,
      libraries: js.Array[String] = js.Array()
  ): js.Dynamic = asJs(Repl(settings.toOption.map(makeSettings), None, libraries.toList))

  private def makeSettings(opts: js.Dynamic): NodeConnectionSettings =
    NodeConnectionSettings(
      opts.nodeUrl.asInstanceOf[String],
      opts.chainId.asInstanceOf[String].charAt(0).toByte,
      opts.address.asInstanceOf[String]
    )

  private def asJs(repl: Repl): js.Dynamic =
    jObj(
      "evaluate"    -> (repl.execute andThen mapResult),
      "info"        -> repl.info,
      "totalInfo"   -> (() => repl.totalInfo),
      "clear"       -> (() => repl.clear()),
      "reconfigure" -> ((opts: js.Dynamic) => asJs(repl.reconfigure(makeSettings(opts))))
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
