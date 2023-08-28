package com.wavesplatform.state.diffs

import cats.implicits.catsSyntaxSemigroup
import com.google.protobuf.ByteString
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockRewardDistribution
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients}
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.transaction.EthereumTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult

object EthereumTransactionDiff {
  def meta(blockchain: Blockchain)(tx: EthereumTransaction): StateSnapshot = {
    val resultEi = tx.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          _       <- if (blockchain.isFeatureActivated(BlockRewardDistribution)) et.checkAsset(tx.underlying.getData) else Right(())
          assetId <- et.tryResolveAsset(blockchain)
        } yield StateSnapshot(
          ethereumTransactionMeta = Map(
            tx.id() -> EthereumTransactionMeta(
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
          invocation <- ei.toInvokeScriptLike(tx, blockchain)
        } yield StateSnapshot(
          ethereumTransactionMeta = Map(
            tx.id() -> EthereumTransactionMeta(
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
    resultEi.getOrElse(StateSnapshot.empty)
  }

  def apply(blockchain: Blockchain, currentBlockTs: Long, limitedExecution: Boolean, enableExecutionLog: Boolean)(
      tx: EthereumTransaction
  ): TracedResult[ValidationError, StateSnapshot] = {
    val baseDiff = tx.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          _        <- checkLeadingZeros(tx, blockchain)
          _        <- TracedResult { if (blockchain.isFeatureActivated(BlockRewardDistribution)) et.checkAsset(tx.underlying.getData) else Right(()) }
          asset    <- TracedResult(et.tryResolveAsset(blockchain))
          transfer <- TracedResult(et.toTransferLike(tx, blockchain))
          assetSnapshot <- TransactionDiffer.assetsVerifierDiff(
            blockchain,
            transfer,
            verify = true,
            StateSnapshot(),
            Int.MaxValue,
            enableExecutionLog
          )
          snapshot <- TransferDiff(blockchain)(tx.senderAddress(), et.recipient, et.amount, asset, tx.fee, tx.feeAssetId)
        } yield assetSnapshot |+| snapshot

      case ei: EthereumTransaction.Invocation =>
        for {
          _          <- checkLeadingZeros(tx, blockchain)
          invocation <- TracedResult(ei.toInvokeScriptLike(tx, blockchain))
          snapshot   <- InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution, enableExecutionLog)(invocation)
          resultSnapshot <- TransactionDiffer.assetsVerifierDiff(
            blockchain,
            invocation,
            verify = true,
            snapshot,
            Int.MaxValue,
            enableExecutionLog
          )
        } yield snapshot.copy(scriptsComplexity = resultSnapshot.scriptsComplexity)
    }

    baseDiff.map(_ |+| meta(blockchain)(tx))
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
