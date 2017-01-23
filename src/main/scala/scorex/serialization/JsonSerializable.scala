package scorex.serialization

import play.api.libs.json.JsObject

trait JsonSerializable {

  def json: JsObject
}
