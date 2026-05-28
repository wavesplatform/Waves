package com.wavesplatform.api.http.requests

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}

trait TxBroadcastRequest[+T <: Transaction & ProvenTransaction] {
  def toTx: Either[ValidationError, T]
}
