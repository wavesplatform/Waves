package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import cats.implicits._

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    DiffsCommon.countScriptsComplexity(blockchain, tx)
      .bimap(
        GenericError(_),
        complexity => Diff(
          height,
          tx,
          portfolios = Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap)),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
          scriptsComplexity = complexity
        )
      )
  }
}
