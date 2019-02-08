package com.wavesplatform.transaction.protobuf

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.Transaction.Data.Data._
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.{AssetId, Proofs => VanillaProofs, Transaction => VanillaTransaction}

trait PBTransactionImplicits {
  private[this] val WavesAssetId = ByteStr.empty
  private[this] val FeeAssetId = WavesAssetId

  implicit class VanillaTransactionImplicitConversionOps(tx: VanillaTransaction) {
    def toPBTransaction: Transaction = tx match {
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val data = MassTransferData(transfers.map(pt => MassTransferData.Transfer(pt.address, pt.amount)))
        val body = Transaction.Body(assetId, sender, timestamp, fee, FeeAssetId, attachment, MassTransfer(data))
        Transaction(Some(body), proofs)

      case _ =>
        throw new IllegalArgumentException(s"Unsupported transaction: $tx")
    }
  }

  implicit class PBTransactionImplicitConversionOps(tx: Transaction) {
    def toVanillaTransaction(version: Int = 2): VanillaTransaction = tx.getBody.getData.data match {
      case MassTransfer(MassTransferData(transfers)) =>
        MassTransferTransaction(
          tx.getBody.assetId,
          tx.getBody.sender,
          transfers.map(t => ParsedTransfer(t.address, t.amount)).toList,
          tx.timestamp,
          tx.getBody.fee,
          tx.getBody.getAttachment.attachments.headOption.fold(ByteStr.empty)(_.data).arr,
          tx.proofs
        )
    }
  }

  private[this] implicit def implicitAssetIdToOption(assetId: AssetId): Option[AssetId] =
    Option(assetId).filterNot(_.isEmpty)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[AssetId]): AssetId =
    assetId.getOrElse(WavesAssetId)

  private[this] implicit def proofsToPBProofs(proofs: VanillaProofs): Option[Transaction.Proofs] =
    Some(Transaction.Proofs(proofs.proofs.map(bs => Transaction.Proofs.Proof(bs))))

  private[this] implicit def attachmentToPBAttachments(attachment: Array[Byte]): Option[Transaction.Attachments] =
    Option(attachment)
      .filter(_.nonEmpty)
      .map(data => Transaction.Attachments(Seq(Transaction.Attachments.Attachment(ByteStr(data)))))

  private[this] implicit def wrapTransactionBodyData(txData: Transaction.Data.Data): Some[Transaction.Data] =
    Some(Transaction.Data(txData))
}

object PBTransactionImplicits extends PBTransactionImplicits