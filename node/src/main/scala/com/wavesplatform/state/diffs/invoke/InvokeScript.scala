package com.wavesplatform.state.diffs.invoke

import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.account._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.Asset.IssuedAsset

trait InvokeScriptLike {
    def dAppAddressOrAlias: AddressOrAlias
    def funcCall: FUNCTION_CALL
    def payments: Seq[Payment]
    def root: InvokeScriptTransaction
    def enableEmptyKeys: Boolean = root.isProtobufVersion
    def checkedAssets: Seq[IssuedAsset] = payments collect { case Payment(_, assetId: IssuedAsset) => assetId }
    def senderAddress: Address
    def sender: PublicKey
}

case class InvokeScript(
     senderDApp: Address,
     sender: PublicKey,
     dAppAddress: Address,
     funcCall: FUNCTION_CALL,
     payments: Seq[Payment],
     root: InvokeScriptTransaction) extends InvokeScriptLike {
    def dAppAddressOrAlias: AddressOrAlias = dAppAddress
    def senderAddress: Address = senderDApp
}
