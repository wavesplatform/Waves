package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

object GenesisTransactionDiff {
  def apply(b: Blockchain)(tx: GenesisTransaction): Either[ValidationError, StateSnapshot] = {
    if (b.height != 1)
      Left(GenericError(s"GenesisTransaction cannot appear in non-initial block (${b.height})"))
    else
      StateSnapshot.build(b, Map(tx.recipient -> Portfolio(balance = tx.amount.value)))
  }
}
