package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.base.CreateAliasTxBase
import scorex.transaction.validation.ValidationError

import scala.util.Right

object CreateAliasTxDiff {
  def apply(height: Int)(tx: CreateAliasTxBase): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        aliases = Map(tx.alias               -> tx.sender.toAddress)
      ))
  }
}
