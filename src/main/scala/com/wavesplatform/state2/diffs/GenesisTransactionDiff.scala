package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{GenesisTransaction, ValidationError}

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.recipient -> Portfolio(
          balance = tx.amount,
          LeaseBalance.empty,
          assets = Map.empty))))
  }
}
