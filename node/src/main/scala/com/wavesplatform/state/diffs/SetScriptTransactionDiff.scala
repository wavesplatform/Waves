package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain)(tx: SetScriptTransaction): Either[ValidationError, Diff] =
    DiffsCommon.countScriptComplexity(tx.script, blockchain)
      .map(
        script =>Diff(
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          scripts = Map(tx.sender.toAddress -> script.map(script => ((tx.sender, script._1, script._2)))),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        )
      )
}
