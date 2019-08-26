package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.DataTransaction

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap)),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
      )
    )
  }
}
