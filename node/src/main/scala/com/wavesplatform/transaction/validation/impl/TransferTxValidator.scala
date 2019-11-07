package com.wavesplatform.transaction.validation.impl

import cats.data.{Validated, ValidatedNel}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.validation.{TxValidator, Validated, validateAmount, validateAttachment, validateFee}

object TransferTxValidator extends TxValidator[TransferTransaction] {
  override def validate(transaction: TransferTransaction): ValidatedNel[ValidationError, TransferTransaction] = {
    import transaction._
    Validations.seq(transaction)(
      TxFeeValidator.validate(transaction),
      validateAmount(amount, assetId.maybeBase58Repr.getOrElse("waves")),
      validateAttachment(attachment)
    )
  }
}
