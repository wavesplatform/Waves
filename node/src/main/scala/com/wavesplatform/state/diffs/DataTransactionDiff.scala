package com.wavesplatform.state.diffs

import cats.syntax.either.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.validation.impl.DataTxValidator

object DataTransactionDiff {
  def apply(blockchain: Blockchain)(tx: DataTransaction): Either[ValidationError, StateSnapshot] = {
    val sender = tx.sender.toAddress
    for {
      // Validate data size
      _ <- DataTxValidator.payloadSizeValidation(blockchain, tx).toEither.leftMap(_.head)
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(sender -> Portfolio(-tx.fee.value)),
        accountData = Map(sender -> tx.data.map(item => item.key -> item).toMap)
      )
    } yield snapshot
  }
}
