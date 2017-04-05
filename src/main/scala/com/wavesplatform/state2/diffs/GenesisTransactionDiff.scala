package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, Portfolio}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{GenesisTransaction, StateValidationError}

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[StateValidationError, Diff] = {
    if (height != 1) Left(TransactionValidationError(tx, "GenesisTranaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.recipient -> Portfolio(
          balance = tx.amount,
          effectiveBalance = tx.amount,
          assets = Map.empty))))
  }
}
