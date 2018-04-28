package com.wavesplatform.state.diffs

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.{CreateAliasTransactionV1, ValidationError}

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(height: Int)(tx: CreateAliasTransactionV1): Either[ValidationError, Diff] = {
    Right(
      Diff(height = height,
           tx = tx,
           portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
           aliases = Map(tx.alias               -> tx.sender.toAddress)))
  }
}
