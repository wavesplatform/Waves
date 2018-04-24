package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.modern.CreateAliasTx
import scorex.transaction.validation.ValidationError

import scala.util.Right

object CreateAliasTxDiff {
  def apply(height: Int)(tx: CreateAliasTx): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.header.fee, LeaseBalance.empty, Map.empty)),
        aliases = Map(tx.payload.alias       -> tx.sender.toAddress)
      ))
  }
}
