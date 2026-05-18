package com.wavesplatform.state.diffs

import cats.implicits.{catsSyntaxEither, catsSyntaxEitherObject, catsSyntaxSemigroup}
import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBRecipients}
import com.wavesplatform.state.diffs.invoke.{InvokeDiffsCommon, InvokeScriptTransactionDiff}
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.transaction.EthereumTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult

object EthereumTransactionDiff {
  def meta(blockchain: Blockchain)(tx: EthereumTransaction): StateSnapshot = {
    val resultEi = tx.payload match {
      case et: EthereumTransaction.Transfer =>
        for {
          _       <- et.checkTransferDataSize(blockchain, tx.underlying.getData)
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
          _        <- checkCommonFields(tx, blockchain)
          _        <- TracedResult(et.checkTransferDataSize(blockchain, tx.underlying.getData))
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
          _          <- checkCommonFields(tx, blockchain)
          invocation <- TracedResult(ei.toInvokeScriptLike(tx, blockchain))
          _          <- TracedResult(InvokeDiffsCommon.checkPayments(blockchain, invocation.payments))
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

  private def checkCommonFields(tx: EthereumTransaction, blockchain: Blockchain): TracedResult[ValidationError, Unit] =
    (for {
      _ <- Either.raiseUnless(
        !(tx.signerKeyBigInt().toByteArray.length < EthereumKeyLength) || blockchain.isFeatureActivated(BlockchainFeatures.ConsensusImprovements)
      )("Invalid public key")
      _ <- Either.raiseWhen(
        tx.longChainId()
          .exists(_ != AddressScheme.current.chainId) && blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)
      )(s"Transaction chain ID ${tx.longChainId()} does not match current chain ID ${AddressScheme.current.chainId}")
      _ <- Either.raiseWhen(
        !tx.ecdsaSignature().isCanonical && blockchain.isFeatureActivated(BlockchainFeatures.DeterministicFinality)
      )("Non-canonical ECDSA signature")
    } yield ()).leftMap(GenericError.apply)
}
