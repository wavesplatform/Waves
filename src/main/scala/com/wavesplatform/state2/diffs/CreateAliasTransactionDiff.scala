package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.{CreateAliasTransaction, ValidationError}

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
      aliases = Map(tx.alias -> tx.sender.toAddress)
    ))
  }
}
