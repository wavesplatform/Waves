package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.base.SetScriptTxBase
import scorex.transaction.validation.ValidationError

import scala.util.Right

object SetScriptDiff {
  def apply(height: Int)(tx: SetScriptTxBase): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> tx.script)
      ))
  }
}
