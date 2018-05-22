package com.wavesplatform.it

import com.wavesplatform.state.DataEntry
import com.wavesplatform.it.util._
import scorex.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}

package object sync {
  val fee                        = 0.001.waves
  val leasingFee                 = 0.001.waves
  val massTransferFeePerTransfer = 0.0005.waves
  val transferFee                = fee
  val transferAmount             = 1.waves

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      fee * (dataSize / 1024 + 1)
    } else fee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    fee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }
}
