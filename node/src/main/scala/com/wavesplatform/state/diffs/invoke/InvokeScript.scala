package com.wavesplatform.state.diffs.invoke

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, Transaction, TxAmount, TxTimestamp}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import monix.eval.Coeval

trait InvokeScriptLike {
  def dApp: Recipient
  def funcCall: FUNCTION_CALL
  def payments: Seq[Payment]
  def root: Option[InvokeScriptTransaction]
  def senderAddress: Address
  def sender: PublicKey
}

trait InvokeScriptTransactionLike extends InvokeScriptLike {
  def id: Coeval[ByteStr]
  def fee: TxAmount
  def feeAssetId: Asset
  def transaction: Transaction
}

object InvokeScriptLike {
  implicit class ISLExt(val isl: InvokeScriptLike) extends AnyVal {
    def enableEmptyKeys: Boolean        = isl.root.forall(_.isProtobufVersion)
    def timestamp: TxTimestamp          = isl.root.fold(0L)(_.timestamp)
    def txId: ByteStr                   = isl.root.fold(ByteStr.empty)(_.id())
    def paymentAssets: Seq[IssuedAsset] = isl.payments.collect(IssuedAssets)
  }

  val IssuedAssets: PartialFunction[Payment, IssuedAsset] = { case Payment(_, assetId: IssuedAsset) => assetId }
}

case class InvokeScript(
    senderDApp: WavesAddress,
    sender: PublicKey,
    dAppAddress: WavesAddress,
    funcCall: FUNCTION_CALL,
    payments: Seq[Payment],
    root: Option[InvokeScriptTransaction]
) extends InvokeScriptLike {
  val dApp: Recipient        = dAppAddress
  val senderAddress: Address = senderDApp
}
