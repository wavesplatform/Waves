package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.smart.SetScriptTransaction

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] =
    DiffsCommon.countVerifierComplexity(tx.script, blockchain)
      .map(
        script => Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          scripts = Map(tx.sender.toAddress -> script),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        )
      )
}
