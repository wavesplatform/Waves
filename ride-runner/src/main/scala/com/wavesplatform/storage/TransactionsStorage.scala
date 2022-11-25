package com.wavesplatform.storage

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.{Height, TransactionId}
import com.wavesplatform.storage.actions.AppendResult
import com.wavesplatform.storage.persistent.PersistentCache

class TransactionsStorage[TagT](
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[TransactionId, Height]
) extends HeightStorage[TransactionId, Height, TagT] {
  override def getFromBlockchain(key: TransactionId): Option[Height] = blockchainApi.getTransactionHeight(key)

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def append(height: Int, pbTxId: ByteString): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    super.append(height, txId, Height(height).some)
  }
}
