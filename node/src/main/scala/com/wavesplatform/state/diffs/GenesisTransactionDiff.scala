package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.GenesisTransaction

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty))))
  }
}
