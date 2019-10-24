package com.wavesplatform.transaction.serialization

import com.wavesplatform.transaction.Transaction
import play.api.libs.json.JsObject

trait TxJson[T <: Transaction] {
  def toJson(tx: T): JsObject
}
