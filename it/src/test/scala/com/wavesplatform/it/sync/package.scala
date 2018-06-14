package com.wavesplatform.it

import com.wavesplatform.state.DataEntry
import com.wavesplatform.it.util._

package object sync {
  val minWavesFee                = 0.001.waves
  val leasingFee                 = 0.002.waves
  val smartFee                   = 0.004.waves
  val issueFee                   = 1.waves
  val transferAmount             = 10.waves
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.waves
  val someAssetAmount            = 100000

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minWavesFee * (dataSize / 1024 + 1)
    } else minWavesFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minWavesFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }
}
