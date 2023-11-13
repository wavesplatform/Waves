package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.validation.TxValidator

object TransferTxValidator extends TxValidator[TransferTransaction] {
  override def validate(transaction: TransferTransaction): ValidatedNel[ValidationError, TransferTransaction] = {
    import transaction.*
    V.seq(transaction)(
      V.transferAttachment(attachment),
      V.addressChainId(recipient, chainId)
    )
  }
}
