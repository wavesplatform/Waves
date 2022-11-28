package com.wavesplatform.storage

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.blockchain.{RemoteData, TaggedData}
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.{Height, TransactionId}
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.TransactionPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

class TransactionsStorage[TagT](
    blockchainApi: BlockchainApi,
    persistentCache: TransactionPersistentCache
) extends ScorexLogging {
  storage =>
  protected val memoryCache = mutable.AnyRefMap.empty[TransactionId, TaggedData[RemoteData[Height], TagT]]

  def getFromBlockchain(key: TransactionId): Option[Height] = blockchainApi.getTransactionHeight(key)

  def get(txId: TransactionId, tag: TagT): Option[Height] =
    memoryCache
      .updateWith(txId) {
        case Some(orig) => Some(orig.withTag(tag))
        case None =>
          val cached = persistentCache.getHeight(txId)
          val r =
            if (cached.loaded) cached
            else
              RemoteData
                .loaded(getFromBlockchain(txId))
                .tap(r => persistentCache.setHeight(txId, r)) // TODO #10: double check before set, because we could have an update

          Some(TaggedData(r, Set(tag)))
      }
      .flatMap(_.data.mayBeValue)

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def setHeight(pbTxId: ByteString, height: Int): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    setHeight(txId, Height(height))
  }

  def setHeight(key: TransactionId, height: Height): AppendResult[TagT] = {
    memoryCache.get(key) match {
      case None => AppendResult.ignored
      case Some(orig) =>
        val updated = RemoteData.Cached(height)
        if (updated == orig.data) AppendResult.ignored
        else {
          log.debug(s"Update Transactions($key)")
          persistentCache.setHeight(key, updated)
          memoryCache.update(key, orig.copy(data = updated))
          AppendResult.appended(mkDataKey(key), orig.tags)
        }
    }
  }

  def remove(pbTxId: ByteString): RollbackResult[TagT] = remove(TransactionId(pbTxId.toByteStr))
  def remove(key: TransactionId): RollbackResult[TagT] =
    memoryCache.get(key) match {
      case None => RollbackResult.ignored
      case Some(orig) =>
        log.info(s"Rollback Transactions($key)")
        persistentCache.remove(key)
        RollbackResult.uncertain(mkDataKey(key), orig.tags) // Will be updated later
    }

  // Use only for known before data
  def reload(txId: TransactionId): Unit =
    memoryCache.updateWith(txId) {
      case Some(orig) =>
        log.info(s"Reload Transactions($txId)")
        val loaded = RemoteData.loaded(getFromBlockchain(txId))
        persistentCache.setHeight(txId, loaded)
        orig.copy(data = loaded).some

      case x => x
    }

  final def mkDataKey(key: TransactionId): DataKey = StorageDataKey(key)

  private case class StorageDataKey(txId: TransactionId) extends DataKey {
    override def reload(height: Int): Unit = storage.reload(txId)
  }
}
