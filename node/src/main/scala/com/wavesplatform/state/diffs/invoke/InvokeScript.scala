package com.wavesplatform.state.diffs.invoke

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxTimestamp
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.InvokeTransaction

trait InvokeScriptLike {
  def dAppAddressOrAlias: AddressOrAlias
  def funcCall: FUNCTION_CALL
  def payments: Seq[Payment]
  def root: Option[InvokeTransaction]
  def checkedAssets: Seq[IssuedAsset] = payments collect { case Payment(_, assetId: IssuedAsset) => assetId }
  def senderAddress: Address
  def sender: PublicKey
  val enableEmptyKeys: Boolean

  def txId: ByteStr          = root.map(_.id()).getOrElse(ByteStr.empty)
  val timestamp: TxTimestamp = root.map(_.timestamp).getOrElse(0L)
}

case class InvokeScript(
    senderDApp: Address,
    sender: PublicKey,
    dAppAddress: Address,
    funcCall: FUNCTION_CALL,
    payments: Seq[Payment],
    root: Option[InvokeTransaction]
) extends InvokeScriptLike {
  def dAppAddressOrAlias: AddressOrAlias = dAppAddress
  def senderAddress: Address             = senderDApp
  val enableEmptyKeys: Boolean           = root.forall(_.enableEmptyKeys)
}
