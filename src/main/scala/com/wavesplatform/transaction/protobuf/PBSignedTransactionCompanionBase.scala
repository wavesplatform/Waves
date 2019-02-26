package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount

trait PBSignedTransactionCompanionBase extends PBSignedTransactionImplicits {
  def create(sender: com.wavesplatform.account.PublicKeyAccount = PublicKeyAccount.empty,
             chainId: com.wavesplatform.transaction.protobuf.ChainId = ChainId.empty,
             fee: _root_.scala.Long = 0L,
             feeAssetId: com.wavesplatform.transaction.protobuf.AssetId = AssetId.Waves,
             timestamp: _root_.scala.Long = 0L,
             version: _root_.scala.Int = 0,
             proofsArray: _root_.scala.collection.Seq[com.wavesplatform.common.state.ByteStr] = Nil,
             data: com.wavesplatform.transaction.protobuf.Transaction.Data = com.wavesplatform.transaction.protobuf.Transaction.Data.Empty)
    : SignedTransaction = {
    SignedTransaction(Transaction(chainId, sender, fee, feeAssetId, timestamp, version, data), proofsArray)
  }
}
