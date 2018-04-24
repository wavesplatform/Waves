package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.modern.smart.SetScriptTx
import scorex.transaction.validation.ValidationError

import scala.util.Right

object SetScriptDiff {
  def apply(height: Int)(tx: SetScriptTx): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.header.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> tx.payload.script)
      ))
  }
}
