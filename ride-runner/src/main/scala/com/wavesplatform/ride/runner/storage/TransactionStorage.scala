package com.wavesplatform.ride.runner.storage

import com.github.benmanes.caffeine.cache.Caffeine
import com.google.protobuf.ByteString
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.ride.runner.db.ReadWrite
import com.wavesplatform.ride.runner.stats.KamonCaffeineStats
import com.wavesplatform.ride.runner.storage.persistent.TransactionPersistentCache
import com.wavesplatform.state.{Height, TransactionId}
import com.wavesplatform.utils.ScorexLogging

import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.chaining.scalaUtilChainingOps

// TODO #97 Settings and metrics
class TransactionStorage[TagT](
    settings: ExactWithHeightStorage.Settings,
    blockchainApi: BlockchainApi,
    persistentCache: TransactionPersistentCache
) extends ScorexLogging {
  storage =>

  // TODO ConcurrentHashMap? Should be this unloaded?
  protected val tags = Caffeine
    .newBuilder()
    .recordStats(() => new KamonCaffeineStats("TransactionStorage.tags"))
    .build[TransactionId, Set[TagT]]()

  protected val values = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(settings.maxEntries)
    .recordStats(() => new KamonCaffeineStats("TransactionStorage.values"))
    .build[TransactionId, RemoteData[Height]]()

  // TODO concurrency issue
  def addDependent(key: TransactionId, tag: TagT): Unit = {
    val origTags = tagsOf(key)
    if (!origTags.contains(tag)) tags.put(key, origTags + tag)
  }

  private def tagsOf(key: TransactionId): Set[TagT] = Option(tags.getIfPresent(key)).getOrElse(Set.empty)

  def getFromBlockchain(key: TransactionId): Option[Height] = blockchainApi.getTransactionHeight(key)

  def getUntagged(txId: TransactionId)(implicit ctx: ReadWrite): Option[Height] = get(txId, None)

  def get(txId: TransactionId, tag: TagT)(implicit ctx: ReadWrite): Option[Height] = get(txId, Some(tag))

  def get(txId: TransactionId, tag: Option[TagT])(implicit ctx: ReadWrite): Option[Height] = {
    // TODO here too
    val origTags = tagsOf(txId)
    tag.foreach { tag =>
      if (!origTags.contains(tag)) tags.put(txId, origTags + tag)
    }

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

  // Use case: got a transaction, got a rollback, same transaction on new height
  def setHeight(pbTxId: ByteString, height: Height)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    setHeight(txId, height)
  }

  def setHeight(txId: TransactionId, height: Height)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val tags = tagsOf(txId)
    if (tags.isEmpty) AffectedTags.empty
    else {
      val orig    = Option(values.getIfPresent(txId)).getOrElse(RemoteData.Unknown).orElse(persistentCache.getHeight(txId))
      val updated = RemoteData.Cached(height)
      if (updated == orig) AffectedTags.empty
      else {
        log.debug(s"Update Transactions($txId)")
        persistentCache.setHeight(txId, updated)
        values.put(txId, updated)
        AffectedTags(tagsOf(txId))
      }
    }
  }

  def remove(pbTxId: ByteString)(implicit ctx: ReadWrite): AffectedTags[TagT] = remove(TransactionId(pbTxId.toByteStr))
  def remove(txId: TransactionId)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val tags = tagsOf(txId)
    if (tags.isEmpty) AffectedTags.empty
    else {
      log.debug(s"Rollback $txId")
      persistentCache.remove(txId)
      values.put(txId, RemoteData.Absence)
      AffectedTags(tags)
    }
  }

  def removeFrom(height: Int)(implicit ctx: ReadWrite): Unit = values.invalidateAll(persistentCache.removeAllFrom(height).asJava)
}
