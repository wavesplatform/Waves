package scorex.transaction

import play.api.libs.json.JsObject
import scorex.account.Account


/**
  * A transaction is an atomic state modifier
  */

trait Transaction extends StateChangeReason {
  val fee: Long

  val timestamp: Long
  val signature: Array[Byte]
  val recipient: Account

  /**
    * A transaction could be serialized into JSON
    */
  def json: JsObject


}
