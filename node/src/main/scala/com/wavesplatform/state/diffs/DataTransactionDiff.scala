package com.wavesplatform.state.diffs

import cats.syntax.either._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.validation.impl.DataTxValidator

object DataTransactionDiff {
  def apply(blockchain: Blockchain)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    for {
      // Validate data size
      _ <- DataTxValidator.payloadSizeValidation(blockchain, tx).toEither.leftMap(_.head)
    } yield Diff(
      portfolios = Map(sender  -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
      accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap)),
      scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
    )
  }
}
