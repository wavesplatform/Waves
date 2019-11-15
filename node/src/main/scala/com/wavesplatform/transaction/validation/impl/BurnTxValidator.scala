package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object BurnTxValidator extends TxValidator[BurnTransaction] {
  override def validate(tx: BurnTransaction): ValidatedV[BurnTransaction] = {
    import tx._
    V.seq(tx)(
      V.positiveOrZeroAmount(quantity, "assets"),
      V.fee(fee)
    )
  }
}
