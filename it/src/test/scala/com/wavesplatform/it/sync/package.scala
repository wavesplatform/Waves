package com.wavesplatform.it

import com.wavesplatform.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.wavesplatform.it.util._
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.state._
import com.wavesplatform.utils.Base58

package object sync {
  val smartFee                   = 0.004.waves
  val minFee                     = 0.001.waves
  val leasingFee                 = 0.002.waves
  val issueFee                   = 1.waves
  val burnFee                    = 1.waves
  val sponsorFee                 = 1.waves
  val setAssetScriptFee          = 1.waves
  val setScriptFee               = 0.01.waves
  val transferAmount             = 10.waves
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.waves
  val someAssetAmount            = 9999999999999l
  val matcherFee                 = 0.003.waves
  val orderFee                   = matcherFee
  val smartMatcherFee            = 0.007.waves
  val smartMinFee                = minFee + smartFee

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

  val script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false).explicitGet()._1
  val scriptBase64 = script.bytes.value.base64

  val errNotAllowedByToken = "Transaction is not allowed by token-script"

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      2.toByte,
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      proofs.proofs.map(_.toString),
      tx.script.map(_.bytes().base64)
    )
  }

}
