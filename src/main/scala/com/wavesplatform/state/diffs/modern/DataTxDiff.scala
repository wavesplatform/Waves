package com.wavesplatform.state.diffs.modern

import com.wavesplatform.state.{AccountDataInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.transaction.base.DataTxBase
import scorex.transaction.validation.ValidationError

object DataTxDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: DataTxBase): Either[ValidationError, Diff] = {
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
