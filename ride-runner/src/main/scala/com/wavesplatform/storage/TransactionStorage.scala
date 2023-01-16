package com.wavesplatform.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.google.protobuf.ByteString
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.{Height, TransactionId}
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.TransactionPersistentCache
import com.wavesplatform.utils.ScorexLogging
import kamon.instrumentation.caffeine.KamonStatsCounter

import scala.util.chaining.scalaUtilChainingOps

class TransactionStorage[TagT](
    blockchainApi: BlockchainApi,
    persistentCache: TransactionPersistentCache
) extends ScorexLogging {
  storage =>

  protected val values = Caffeine
    .newBuilder()
    .recordStats(() => new KamonStatsCounter("TransactionStorage.values"))
    .build[TransactionId, RemoteData[Height]]()

  protected val tags = Caffeine
    .newBuilder()
    .recordStats(() => new KamonStatsCounter("TransactionStorage.tags"))
    .build[TransactionId, Set[TagT]]()

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
  def setHeight(pbTxId: ByteString, height: Int): AffectedTags[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    setHeight(txId, Height(height))
  }

  def setHeight(txId: TransactionId, height: Height): AffectedTags[TagT] =
    Option(values.getIfPresent(txId)) match {
      case None => AffectedTags.empty
      case Some(orig) =>
        val updated = RemoteData.Cached(height)
        if (updated == orig) AffectedTags.empty
        else {
          log.debug(s"Update Transactions($txId)")
          persistentCache.setHeight(txId, updated)
          values.put(txId, updated)
          AffectedTags(tagsOf(txId))
        }
    }

  def remove(pbTxId: ByteString): AffectedTags[TagT] = remove(TransactionId(pbTxId.toByteStr))
  def remove(txId: TransactionId): AffectedTags[TagT] =
    Option(values.getIfPresent(txId)) match {
      case None => AffectedTags.empty
      case _ =>
        log.info(s"Rollback Transactions($txId)")
        persistentCache.remove(txId)
        values.put(txId, RemoteData.Absence)
        AffectedTags(tagsOf(txId))
    }

  // Use only for known before data
  def reload(txId: TransactionId): Unit = {
    log.info(s"Invalidating Transactions($txId)")
    values.invalidate(txId)
  }
}
