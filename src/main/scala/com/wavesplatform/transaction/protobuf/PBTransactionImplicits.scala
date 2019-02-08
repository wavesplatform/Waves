package com.wavesplatform.transaction.protobuf

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.Transaction.Data.Data._
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{AssetId, Transaction => VanillaTransaction}

trait PBTransactionImplicits {
  private[this] val WavesAssetId = ByteStr.empty
  private[this] val FeeAssetId   = WavesAssetId

  implicit class VanillaTransactionImplicitConversionOps(tx: VanillaTransaction) {
    def toPBTransaction: Transaction = tx match {
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferData(transfers.map(pt => MassTransferData.Transfer(pt.address, pt.amount)))
        Transaction(assetId, sender, timestamp, fee, FeeAssetId, ByteStr(attachment), MassTransfer(data), proofs.proofs)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBTransactionImplicitConversionOps(tx: Transaction) {
    def toVanillaTransaction(version: Int = 2): VanillaTransaction = tx.getData.data match {
      case MassTransfer(MassTransferData(transfers)) =>
        MassTransferTransaction(
          tx.assetId,
          tx.sender,
          transfers.map(t => ParsedTransfer(t.address, t.amount)).toList,
          tx.timestamp,
          tx.fee,
          tx.attachment.arr,
          tx.getProofs
        )
    }
  }

  private[this] implicit def implicitAssetIdToOption(assetId: AssetId): Option[AssetId] =
    Option(assetId).filterNot(_.isEmpty)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[AssetId]): AssetId =
    assetId.getOrElse(WavesAssetId)

  private[this] implicit def wrapTransactionBodyData(txData: Transaction.Data.Data): Some[Transaction.Data] =
    Some(Transaction.Data(txData))
}

object PBTransactionImplicits extends PBTransactionImplicits
