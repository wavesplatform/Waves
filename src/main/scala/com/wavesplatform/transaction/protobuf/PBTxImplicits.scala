package com.wavesplatform.transaction.protobuf

import com.wavesplatform.transaction.{AssetId, Transaction => VanillaTransaction}

trait PBTxImplicits {
  implicit def implicitAssetIdToOption(assetId: AssetId): Option[AssetId] = Option(assetId).filterNot(_.isEmpty)

  implicit class PBTransactionImplicitConversionOps(tx: Transaction) {
    import com.wavesplatform.transaction.protobuf.Transaction.Data.Data._

    def toVanillaTransaction: VanillaTransaction = tx.getBody.getData.data match {
      case MassTransfer(MassTransferData(transfers)) => MassTransferTransaction(tx.getBody.assetId, tx.getBody.sender, transfers.map(t => ParsedTransfer(t.address)))
    }
  }

}
