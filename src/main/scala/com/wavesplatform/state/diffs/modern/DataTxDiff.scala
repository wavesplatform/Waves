package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{AccountDataInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.transaction.modern.DataTx
import scorex.transaction.validation.ValidationError

object DataTxDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: DataTx): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(
      Diff(
        height,
        tx,
        portfolios = Map(sender  -> Portfolio(-tx.header.fee, LeaseBalance.empty, Map.empty)),
        accountData = Map(sender -> AccountDataInfo(tx.payload.entries.map(item => item.key -> item).toMap))
      ))
  }
}
