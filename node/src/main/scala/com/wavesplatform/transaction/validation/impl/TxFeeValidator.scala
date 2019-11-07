package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.validation.{TxValidator, _}
import com.wavesplatform.transaction.{Transaction, TxWithFee}

object TxFeeValidator extends TxValidator[Transaction with TxWithFee] {
  override def validate(tx: Transaction with TxWithFee): ValidatedV[Transaction with TxWithFee] =
    V.fee(tx.fee).map(_ => tx)
}
