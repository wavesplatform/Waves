package com.wavesplatform.state.diffs

import com.google.protobuf.ByteString
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients}
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.EthereumTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult

object EthereumTransactionDiff {
  def apply(blockchain: Blockchain, currentBlockTs: Long, limitedExecution: Boolean)(e: EthereumTransaction): TracedResult[ValidationError, Diff] = {
    val baseDiff = e.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          _         <- checkLeadingZeros(e, blockchain)
          _         <- TracedResult(et.checkDataSize(blockchain, e.underlying.getData))
          asset     <- TracedResult(et.tryResolveAsset(blockchain))
          transfer  <- TracedResult(et.toTransferLike(e, blockchain))
          assetDiff <- TransactionDiffer.assetsVerifierDiff(blockchain, transfer, verify = true, Diff(), Int.MaxValue)
          diff      <- TransferDiff(blockchain)(e.senderAddress(), et.recipient, et.amount, asset, e.fee, e.feeAssetId)
          result    <- assetDiff.combineE(diff)
        } yield result

      case ei: EthereumTransaction.Invocation =>
        for {
          _          <- checkLeadingZeros(e, blockchain)
          invocation <- TracedResult(ei.toInvokeScriptLike(e, blockchain))
          diff       <- InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution)(invocation)
          result     <- TransactionDiffer.assetsVerifierDiff(blockchain, invocation, verify = true, diff, Int.MaxValue)
        } yield result
    }

    baseDiff.flatMap(bd => TracedResult(bd.combineE(this.meta(blockchain)(e))))
  }

  def meta(blockchain: Blockchain)(e: EthereumTransaction): Diff = {
    val resultEi = e.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          _       <- et.checkDataSize(blockchain, e.underlying.getData)
          assetId <- et.tryResolveAsset(blockchain)
        } yield Diff(
          ethereumTransactionMeta = Map(
            e.id() -> EthereumTransactionMeta(
              EthereumTransactionMeta.Payload.Transfer(
                EthereumTransactionMeta.Transfer(
                  ByteString.copyFrom(PBRecipients.publicKeyHash(et.recipient)),
                  Some(PBAmounts.fromAssetAndAmount(assetId, et.amount))
                )
              )
            )
          )
        )

      case ei: EthereumTransaction.Invocation =>
        for {
          invocation <- ei.toInvokeScriptLike(e, blockchain)
        } yield Diff(
          ethereumTransactionMeta = Map(
            e.id() -> EthereumTransactionMeta(
              EthereumTransactionMeta.Payload.Invocation(
                EthereumTransactionMeta.Invocation(
                  ByteString.copyFrom(SerdeV1.serialize(invocation.funcCall)),
                  invocation.payments.map(p => PBAmounts.fromAssetAndAmount(p.assetId, p.amount))
                )
              )
            )
          )
        )
    }
    resultEi.getOrElse(Diff.empty)
  }

  private def checkLeadingZeros(tx: EthereumTransaction, blockchain: Blockchain): TracedResult[ValidationError, Unit] = {
    TracedResult(
      Either.cond(
        !(tx.signerKeyBigInt().toByteArray.length < EthereumKeyLength) || blockchain.isFeatureActivated(BlockchainFeatures.ConsensusImprovements),
        (),
        GenericError("Invalid public key")
      )
    )
  }
}
