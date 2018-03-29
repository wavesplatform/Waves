package scorex.serialization

import monix.eval.Coeval
import play.api.libs.json.JsObject
import scorex.account.AddressScheme

trait JsonSerializable {

  val json: Coeval[JsObject]
}

trait TxJsonSerializable {

  def json(implicit addressScheme: AddressScheme): JsObject
}
