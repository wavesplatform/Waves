package scorex.transaction

import play.api.libs.json.JsObject


/**
  * A transaction is an atomic state modifier
  */

trait Transaction {
  val fee: Long

  val timestamp: Long
  val signature: Array[Byte]

  /**
    * A transaction could be serialized into JSON
    */
  def json(): JsObject

  /**
    * A transaction could be serialized into binary form
    */
  def bytes(): Array[Byte]
}
