package com.wavesplatform.state2.diffs

import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.transaction.{DataTransaction, ValidationError}

object DataTransactionDiff {

  def apply(state: SnapshotStateReader, height: Int)(tx: DataTransaction): Either[ValidationError, Diff] = {
    val sender = tx.sender.toAddress
    Right(Diff(height, tx,
      portfolios = Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
      accountData = Map(sender -> AccountDataInfo(tx.data.map(item => item.key -> item).toMap))))
  }
}