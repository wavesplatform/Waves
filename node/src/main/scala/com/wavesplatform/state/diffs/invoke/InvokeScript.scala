package com.wavesplatform.state.diffs.invoke

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxTimestamp
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

trait InvokeScriptLike {
  def dAppAddressOrAlias: AddressOrAlias
  def funcCall: FUNCTION_CALL
  def payments: Seq[Payment]
  def root: Option[InvokeScriptTransaction]
  def checkedAssets: Seq[IssuedAsset] = payments collect { case Payment(_, assetId: IssuedAsset) => assetId }
  def senderAddress: Address
  def sender: PublicKey
}

object InvokeScriptLike {
  implicit class InvokeScriptLikeOps(val invokeScriptLike: InvokeScriptLike) extends AnyVal {
    def txId: ByteStr            = invokeScriptLike.root.map(_.id()).getOrElse(ByteStr.empty)
    def enableEmptyKeys: Boolean = invokeScriptLike.root.forall(_.isProtobufVersion)
    def timestamp: TxTimestamp   = invokeScriptLike.root.map(_.timestamp).getOrElse(0L)
  }
}

case class InvokeScript(
    senderDApp: Address,
    sender: PublicKey,
    dAppAddress: Address,
    funcCall: FUNCTION_CALL,
    payments: Seq[Payment],
    root: Option[InvokeScriptTransaction]
) extends InvokeScriptLike {
  def dAppAddressOrAlias: AddressOrAlias = dAppAddress
  def senderAddress: Address             = senderDApp
}
