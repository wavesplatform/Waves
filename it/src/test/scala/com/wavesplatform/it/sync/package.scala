package com.wavesplatform.it

import com.wavesplatform.state.DataEntry
import com.wavesplatform.it.util._

package object sync {
  val smartFee                   = 0.004.waves
  val minFee                     = 0.001.waves
  val leasingFee                 = 0.002.waves
  val issueFee                   = 1.waves
  val burnFee                    = 1.waves
  val sponsorFee                 = 1.waves
  val setScriptFee               = 0.01.waves
  val transferAmount             = 10.waves
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.waves
  val someAssetAmount            = 9999999999999l
  val matcherFee                 = 0.003.waves

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1
}
