package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.{CreateAliasTransaction, ValidationError}

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAccount -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      aliases = Map(tx.alias -> tx.sender.toAccount)
    ))
  }
}
