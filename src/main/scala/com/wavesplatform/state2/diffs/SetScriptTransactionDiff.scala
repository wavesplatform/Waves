package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseBalance, Portfolio}
import scorex.transaction.ValidationError
import scorex.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {Right(Diff(
    height = height,
    tx = tx,
    portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
    scripts = Map(tx.sender.toAddress -> tx.script)
  ))}
}
