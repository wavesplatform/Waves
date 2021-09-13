package com.wavesplatform.state.diffs

import com.google.protobuf.ByteString
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients}
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.EthereumTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult

object EthereumTransactionDiff {
  def apply(blockchain: Blockchain, currentBlockTs: Long)(e: EthereumTransaction): TracedResult[ValidationError, Diff] =
    e.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          asset <- et.tryResolveAsset(blockchain)
          diff  <- TransferDiff(blockchain)(e.senderAddress(), et.recipient, et.amount, asset, e.fee, e.feeAssetId)
        } yield
          diff.copy(
            ethereumTransactionMeta = Map(
              e.id() -> EthereumTransactionMeta(
                EthereumTransactionMeta.Payload.Transfer(
                  EthereumTransactionMeta.Transfer(
                    ByteString.copyFrom(PBRecipients.publicKeyHash(et.recipient)),
                    Some(PBAmounts.fromAssetAndAmount(asset, et.amount))
                  )
                )
              )
            )
          )

      case ei: EthereumTransaction.Invocation =>
        for {
          invocation <- TracedResult(ei.toInvokeScriptLike(e, blockchain))
          diff       <- InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution = true)(invocation)
        } yield
          diff.copy(
            ethereumTransactionMeta = Map(
              e.id() -> EthereumTransactionMeta(
                EthereumTransactionMeta.Payload.Invocation(
                  EthereumTransactionMeta.Invocation(
                    ByteString.copyFrom(Serde.serialize(invocation.funcCall)),
                    invocation.payments.map(p => PBAmounts.fromAssetAndAmount(p.assetId, p.amount))
                  )
                )
              )
            )
          )
    }
}
