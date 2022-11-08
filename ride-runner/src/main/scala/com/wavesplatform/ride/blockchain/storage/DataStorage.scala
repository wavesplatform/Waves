package com.wavesplatform.ride.blockchain.storage

import cats.syntax.option.*
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.ride.blockchain.*
import com.wavesplatform.ride.blockchain.caches.PersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable
import scala.util.chaining.*

trait DataStorage[KeyT <: AnyRef, ValueT, TagT] extends ScorexLogging {
  protected val memoryCache = mutable.AnyRefMap.empty[KeyT, TaggedData[BlockchainData[ValueT], TagT]]

  lazy val name = getSimpleName(this)

  def mkDataKey(key: KeyT): DataKey

  def getFromBlockchain(key: KeyT): Option[ValueT]

  def persistentCache: PersistentCache[KeyT, ValueT]

  def get(height: Int, key: KeyT, tag: TagT): Option[ValueT] =
    memoryCache
      .updateWith(key) {
        case Some(orig) => Some(orig.withTag(tag))
        case None =>
          val cached = persistentCache.get(height, key)
          val r =
            if (cached.loaded) cached
            else BlockchainData.loaded(getFromBlockchain(key)).tap(r => persistentCache.set(height, key, r))

          Some(TaggedData(r, Set(tag)))
      }
      .flatMap(_.data.mayBeValue)

  // Use only for known before data
  def reload(height: Int, key: KeyT): Unit =
    memoryCache.updateWith(key) {
      case Some(orig) =>
        log.info(s"Reload $name($key)")

        val loaded = BlockchainData.loaded(getFromBlockchain(key))
        persistentCache.set(height, key, loaded)
        orig.copy(data = loaded).some

      case x => x
    }

  def append(height: Int, key: KeyT, update: Option[ValueT]): AppendResult[TagT] =
    memoryCache.get(key) match {
      case None => AppendResult.ignored
      case Some(orig) =>
        log.debug(s"Update $name($key)")

        val updated = BlockchainData.loaded(update)
        persistentCache.set(height, key, updated)

        if (updated == orig.data) AppendResult.ignored
        else {
          memoryCache.update(key, orig.copy(data = updated))
          AppendResult.appended(mkDataKey(key), orig.tags)
        }
    }

  // Micro blocks don't affect, because we know new values
  // TODO rollbackAssetId?
  def rollback(rollbackHeight: Int, key: KeyT, after: Option[ValueT]): RollbackResult[TagT] = {
    memoryCache.get(key) match {
      case None => RollbackResult.ignored
      case Some(orig) =>
        log.info(s"Rollback $name($key)")

        persistentCache.remove(rollbackHeight + 1, key) match {
          case latest @ BlockchainData.Cached(_) =>
            // TODO compare with afterRollback
            memoryCache.update(key, orig.copy(data = latest))
            RollbackResult.rolledBack(orig.tags)

          case BlockchainData.Unknown | BlockchainData.Absence =>
            after match {
              case None => RollbackResult.uncertain(mkDataKey(key), orig.tags) // will be updated later
              case Some(after) =>
                val x = BlockchainData.Cached(after)
                persistentCache.set(rollbackHeight, key, x)
                memoryCache.update(key, orig.copy(data = x))
                RollbackResult.rolledBack(orig.tags)
            }
        }
    }
  }
}
