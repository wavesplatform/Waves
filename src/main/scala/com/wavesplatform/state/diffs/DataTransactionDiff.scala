package com.wavesplatform.state.diffs

import com.wavesplatform.state._
import scorex.transaction.ValidationError
import scorex.transaction.data.DataTransaction
import cats.implicits._

object DataTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress

    val isRecipientValid = tx.recipient
      .map(blockchain.resolveAliasEi)
      .fold(true)(_.isRight)

    tx.recipient
      .map(aoa => blockchain.resolveAliasEi(aoa).map(_ => ()))
      .getOrElse(().asRight[ValidationError])
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
