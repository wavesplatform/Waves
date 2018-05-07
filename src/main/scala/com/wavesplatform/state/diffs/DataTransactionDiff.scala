package com.wavesplatform.state.diffs

import com.wavesplatform.state._
import scorex.transaction.ValidationError
import scorex.transaction.data.DataTransactionV1

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransactionV1): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(sender  -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
      ))
  }
}
