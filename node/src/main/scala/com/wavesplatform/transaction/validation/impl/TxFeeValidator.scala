package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction, TxWithFee}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation._

object TxFeeValidator extends TxValidator[Transaction with TxWithFee] {
  override def validate(tx: Transaction with TxWithFee): ValidatedNel[ValidationError, Transaction with TxWithFee] = {
    Validations.seq(tx)(validateFee(tx.fee))
  }
}
