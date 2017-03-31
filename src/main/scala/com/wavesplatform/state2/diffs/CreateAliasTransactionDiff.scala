package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, Portfolio}
import scorex.transaction.{CreateAliasTransaction, StateValidationError}

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(height: Int)(tx: CreateAliasTransaction): Either[StateValidationError, Diff] = {
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAccount -> Portfolio(-tx.fee, -tx.fee, Map.empty)),
      assetInfos = Map.empty,
      aliases = Map(tx.alias -> tx.sender.toAccount)
    ))
  }
}
