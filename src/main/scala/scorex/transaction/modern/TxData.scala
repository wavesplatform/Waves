package scorex.transaction.modern

import monix.eval.Coeval
import play.api.libs.json.JsObject

trait TxData {
  def bytes: Coeval[Array[Byte]]
  def json: Coeval[JsObject]
}
