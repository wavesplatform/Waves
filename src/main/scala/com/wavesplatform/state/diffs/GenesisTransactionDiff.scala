package com.wavesplatform.state.diffs

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.GenesisTransaction
import scorex.transaction.validation.ValidationError

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty))))
  }
}
