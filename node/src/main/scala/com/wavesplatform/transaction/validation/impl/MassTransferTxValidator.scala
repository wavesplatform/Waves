package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.TxValidationError.{GenericError, NegativeAmount}
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object MassTransferTxValidator extends TxValidator[MassTransferTransaction] {
  override def validate(tx: MassTransferTransaction): ValidatedV[MassTransferTransaction] = {
    import tx._
    val negativeAmount = transfers.find(_.amount < 0)
    V.seq(tx)(
      V.noOverflow(fee +: transfers.map(_.amount): _*),
      V.cond(transfers.length <= MaxTransferCount, GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount")),
      V.transferAttachment(isProtobufVersion, attachment),
      V.cond(negativeAmount.isEmpty, NegativeAmount(negativeAmount.get.amount, "One of the transfers has negative amount")),
      V.fee(fee),
      V.chainIds(chainId, transfers.map(_.address.chainId): _*)
    )
  }
}
