package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.Transaction.Body

trait PBTransactionCompanionBase extends PBTransactionImplicits with PBTransactionJson with PBTransactionParser {
  def create(sender: PublicKeyAccount = PublicKeyAccount(Array.emptyByteArray),
             chainId: Int = 0,
             fee: Long = 0L,
             feeAssetId: PBAssetId = AssetId.Waves,
             timestamp: Long = 0L,
             version: Int = 0,
             data: Body.Data = com.wavesplatform.transaction.protobuf.Transaction.Body.Data.Empty,
             proofsArray: Seq[VanillaAssetId] = _root_.scala.collection.Seq.empty): Transaction = {

    val body = Transaction.Body(sender, chainId, fee, feeAssetId, timestamp, version, data)
    Transaction(Some(body), proofsArray)
  }

  def unapply(tx: Transaction): Option[(PublicKeyAccount, Int, Long, PBAssetId, Long, Int, Transaction.Body.Data, Seq[ByteStr])] = {
    val body = tx.getBody
    import body._
    Some((sender, chainId, fee, feeAssetId, timestamp, body.version, data, tx.proofsArray))
  }

  implicit def extractTransactionBody(tx: Transaction): Transaction.Body = tx.getBody
}
