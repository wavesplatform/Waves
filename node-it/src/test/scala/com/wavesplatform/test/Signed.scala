package com.wavesplatform.test

import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Asset, Proofs, TxTimestamp, TransactionSignOps}

object Signed {
  def invokeScript(
      version: Byte,
      sender: KeyPair,
      dApp: AddressOrAlias,
      functionCall: Option[Terms.FUNCTION_CALL],
      payments: Seq[InvokeScriptTransaction.Payment],
      fee: Long,
      feeAssetId: Asset,
      timestamp: TxTimestamp
  ): InvokeScriptTransaction =
    InvokeScriptTransaction
      .create(version, sender.publicKey, dApp, functionCall, payments, fee, feeAssetId, timestamp, Proofs.empty, dApp.chainId)
      .map(_.signWith(sender.privateKey))
      .explicitGet()
}
