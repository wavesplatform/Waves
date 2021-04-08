package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError(s"GenesisTransaction cannot appear in non-initial block ($height)"))
    else
      Right(Diff(portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount))))
  }
}
