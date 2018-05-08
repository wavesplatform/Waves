package com.wavesplatform.state.diffs

import com.wavesplatform.state._
import scorex.transaction.ValidationError
import scorex.transaction.data.DataTransaction
import cats.implicits._
import scorex.account.Address

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress

    tx.recipient
      .traverse[Either[ValidationError, ?], Address](blockchain.resolveAliasEi)
      .map(_ => {
        Diff(
          height,
          tx,
          portfolios = Map(sender  -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))
        )
      })
  }
}
