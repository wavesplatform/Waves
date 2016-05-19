package scorex.transaction

import scorex.account.Account
import scorex.serialization.JsonSerializable


/**
  * A transaction is an atomic state modifier
  */

trait Transaction extends StateChangeReason with JsonSerializable {
  val fee: Long

  val timestamp: Long
  val recipient: Account

}
