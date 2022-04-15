package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object MassTransferTxValidator extends TxValidator[MassTransferTransaction] {
  override def validate(tx: MassTransferTransaction): ValidatedV[MassTransferTransaction] = {
    import tx.*
    V.seq(tx)(
      V.noOverflow((fee.value +: transfers.map(_.amount.value))*),
      V.cond(transfers.length <= MaxTransferCount, GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount")),
      V.transferAttachment(attachment),
      V.chainIds(chainId, transfers.view.map(_.address).collect {
        case wa: Address => wa.chainId
        case wl: Alias        => wl.chainId
      }.toSeq*)
    )
  }
}
