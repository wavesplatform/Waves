package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.GenesisTransaction

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError(s"GenesisTransaction cannot appear in non-initial block ($height)"))
    else
      Right(Diff(tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty))))
  }
}
