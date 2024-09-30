package com.wavesplatform.transaction.validation.impl

import cats.syntax.validated.*
import com.wavesplatform.transaction.assets.BurnTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object BurnTxValidator extends TxValidator[BurnTransaction] {
  override def validate(tx: BurnTransaction): ValidatedV[BurnTransaction] = tx.validNel
}
