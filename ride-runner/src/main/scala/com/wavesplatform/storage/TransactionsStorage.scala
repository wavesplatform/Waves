package com.wavesplatform.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.google.protobuf.ByteString
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.{Height, TransactionId}
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.TransactionPersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.scalaUtilChainingOps

class TransactionsStorage[TagT](
    blockchainApi: BlockchainApi,
    persistentCache: TransactionPersistentCache
) extends ScorexLogging {
  storage =>

  protected val values = Caffeine.newBuilder().build[TransactionId, RemoteData[Height]]()

  protected val tags                                = Caffeine.newBuilder().build[TransactionId, Set[TagT]]()
  private def tagsOf(key: TransactionId): Set[TagT] = Option(tags.getIfPresent(key)).getOrElse(Set.empty)

  def getFromBlockchain(key: TransactionId): Option[Height] = blockchainApi.getTransactionHeight(key)

  def get(txId: TransactionId, tag: TagT): Option[Height] = {
    val origTags = tagsOf(txId)
    if (!origTags.contains(tag)) tags.put(txId, origTags + tag)

    values
      .get(
        txId,
        { (txId: TransactionId) =>
          val cached = persistentCache.getHeight(txId)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(getFromBlockchain(txId))
              .tap(r => persistentCache.setHeight(txId, r))
        }
      )
      .mayBeValue
  }

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def setHeight(pbTxId: ByteString, height: Int): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    setHeight(txId, Height(height))
  }

  def setHeight(txId: TransactionId, height: Height): AppendResult[TagT] =
    Option(values.getIfPresent(txId)) match {
      case None => AppendResult.ignored
      case Some(orig) =>
        val updated = RemoteData.Cached(height)
        if (updated == orig) AppendResult.ignored
        else {
          log.debug(s"Update Transactions($txId)")
          persistentCache.setHeight(txId, updated)
          values.put(txId, updated)
          AppendResult.appended(mkDataKey(txId), tagsOf(txId))
        }
    }

  def remove(pbTxId: ByteString): RollbackResult[TagT] = remove(TransactionId(pbTxId.toByteStr))
  def remove(txId: TransactionId): RollbackResult[TagT] =
    Option(values.getIfPresent(txId)) match {
      case None => RollbackResult.ignored
      case _ =>
        log.info(s"Rollback Transactions($txId)")
        persistentCache.remove(txId)
        RollbackResult.uncertain(mkDataKey(txId), tagsOf(txId)) // Will be updated later
    }

  // Use only for known before data
  def reload(txId: TransactionId): Unit = {
    log.info(s"Invalidating Transactions($txId)")
    values.invalidate(txId)
  }

  final def mkDataKey(key: TransactionId): DataKey = StorageDataKey(key)

  private case class StorageDataKey(txId: TransactionId) extends DataKey {
    override def reload(height: Int): Unit = storage.reload(txId)
  }
}
