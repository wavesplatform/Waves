package com.wavesplatform.storage

import cats.syntax.option.*
import com.wavesplatform.blockchain.{RemoteData, TaggedData}
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable
import scala.util.chaining.*

trait HeightStorage[KeyT <: AnyRef, ValueT, TagT] extends ScorexLogging { storage =>
  protected val memoryCache = mutable.AnyRefMap.empty[KeyT, TaggedData[RemoteData[ValueT], TagT]]

  lazy val name = getSimpleName(this)

  def getFromBlockchain(key: KeyT): Option[ValueT]

  def persistentCache: PersistentCache[KeyT, ValueT]

  def getUntagged(height: Int, key: KeyT): Option[ValueT]    = getInternal(height, key, None)
  def get(height: Int, key: KeyT, tag: TagT): Option[ValueT] = getInternal(height, key, Some(tag))
  private def getInternal(height: Int, key: KeyT, tag: Option[TagT]): Option[ValueT] =
    memoryCache
      .updateWith(key) {
        case Some(orig) => Some(tag.foldLeft(orig)(_.withTag(_)))
        case None =>
          val cached = persistentCache.get(height, key)
          val r =
            if (cached.loaded) cached
            else
              RemoteData
                .loaded(getFromBlockchain(key))
                .tap(r => persistentCache.set(height, key, r)) // TODO double check before set, because we could have an update

          Some(TaggedData(r, tag.toSet))
      }
      .flatMap(_.data.mayBeValue)

  // Use only for known before data
  def reload(height: Int, key: KeyT): Unit =
    memoryCache.updateWith(key) {
      case Some(orig) =>
        log.info(s"Reload $name($key)")

        val loaded = RemoteData.loaded(getFromBlockchain(key))
        persistentCache.set(height, key, loaded)
        orig.copy(data = loaded).some

      case x => x
    }

  def append(height: Int, key: KeyT, update: Option[ValueT]): AppendResult[TagT] =
    memoryCache.get(key) match {
      case None => AppendResult.ignored
      case Some(orig) =>
        log.debug(s"Update $name($key)")

        val updated = RemoteData.loaded(update)
        persistentCache.set(height, key, updated) // Write here (not in "else"), because we want to preserve the height

        if (updated == orig.data) AppendResult.ignored
        else {
          memoryCache.update(key, orig.copy(data = updated))
          AppendResult.appended(mkDataKey(key), orig.tags)
        }
    }

  // Micro blocks don't affect, because we know new values
  // TODO rollbackAssetId?
  def rollback(rollbackHeight: Int, key: KeyT, after: Option[ValueT]): RollbackResult[TagT] =
    memoryCache.get(key) match {
      case None => RollbackResult.ignored
      case Some(orig) =>
        log.info(s"Rollback $name($key)")

        persistentCache.remove(rollbackHeight + 1, key) match {
          case latest @ RemoteData.Cached(_) =>
            // TODO compare with afterRollback
            memoryCache.update(key, orig.copy(data = latest))
            RollbackResult.rolledBack(orig.tags)

          case RemoteData.Unknown | RemoteData.Absence =>
            after match {
              case None => RollbackResult.uncertain(mkDataKey(key), orig.tags) // will be updated later
              case Some(after) =>
                val x = RemoteData.Cached(after)
                persistentCache.set(rollbackHeight, key, x)
                memoryCache.update(key, orig.copy(data = x))
                RollbackResult.rolledBack(orig.tags)
            }
        }
    }

  final def mkDataKey(key: KeyT): DataKey = StorageDataKey(key)

  private case class StorageDataKey(key: KeyT) extends DataKey {
    override def reload(height: Int): Unit = storage.reload(height, key)
  }
}
