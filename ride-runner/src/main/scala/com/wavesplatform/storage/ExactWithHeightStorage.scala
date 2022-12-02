package com.wavesplatform.storage

import cats.syntax.option.*
import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.utils.ScorexLogging

import scala.util.chaining.*

/** Exact, because stores not only a value, but an absence of it too
  */
trait ExactWithHeightStorage[KeyT <: AnyRef, ValueT, TagT] extends ScorexLogging {
  storage =>
  protected val values = Caffeine.newBuilder().build[KeyT, RemoteData[ValueT]]()

  protected val tags                       = Caffeine.newBuilder().build[KeyT, Set[TagT]]()
  private def tagsOf(key: KeyT): Set[TagT] = Option(tags.getIfPresent(key)).getOrElse(Set.empty)

  lazy val name = getSimpleName(this)

  def getFromBlockchain(key: KeyT): Option[ValueT]

  def persistentCache: PersistentCache[KeyT, ValueT]

  def getUntagged(height: Int, key: KeyT): Option[ValueT] = getInternal(height, key, None)

  def get(height: Int, key: KeyT, tag: TagT): Option[ValueT] = getInternal(height, key, Some(tag))

  private def getInternal(height: Int, key: KeyT, tag: Option[TagT]): Option[ValueT] = {
    tag.foreach { tag =>
      val origTags = tagsOf(key)
      if (!origTags.contains(tag)) tags.put(key, origTags + tag)
    }

    values
      .get(
        key,
        { (key: KeyT) =>
          val cached = persistentCache.get(height, key)
          if (cached.loaded) cached
          else
            RemoteData
              .loaded(getFromBlockchain(key))
              .tap(r => persistentCache.set(height, key, r))
        }
      )
      .mayBeValue
  }

  // Use only for known before data
  def reload(height: Int, key: KeyT): Unit = {
    log.info(s"Invalidating $name($key)")
    values.invalidate(key)
  }

  def append(height: Int, key: KeyT, update: ValueT): AppendResult[TagT] = append(height, key, update.some)

  def append(height: Int, key: KeyT, update: Option[ValueT]): AppendResult[TagT] = {
    Option(values.getIfPresent(key)) match {
      case None => AppendResult.ignored
      case Some(orig) =>
        log.debug(s"Update $name($key)")

        val updated = RemoteData.loaded(update)
        persistentCache.set(height, key, updated) // Write here (not in "else"), because we want to preserve the height

        if (updated == orig) AppendResult.ignored
        else {
          values.put(key, updated)
          AppendResult.appended(mkDataKey(key), tagsOf(key))
        }
    }
  }

  def rollback(rollbackHeight: Int, key: KeyT, after: ValueT): RollbackResult[TagT] = rollback(rollbackHeight, key, after.some)

  // Micro blocks don't affect, because we know new values
  def rollback(rollbackHeight: Int, key: KeyT, after: Option[ValueT]): RollbackResult[TagT] =
    Option(values.getIfPresent(key)) match {
      case None => RollbackResult.ignored
      case Some(orig) =>
        log.info(s"Rollback $name($key)")

        val tags = tagsOf(key)
        persistentCache.remove(rollbackHeight + 1, key) match {
          case RemoteData.Unknown =>
            after match {
              case None => RollbackResult.uncertain(mkDataKey(key), tags) // will be updated later
              case Some(after) =>
                val restored = RemoteData.Cached(after)
                persistentCache.set(rollbackHeight, key, restored)
                values.put(key, restored)
                RollbackResult.rolledBack(tags)
            }

          case latest => // Cached | Absence
            // TODO #11: compare with afterRollback
            values.put(key, latest)
            RollbackResult.rolledBack(tags)
        }
    }

  final def mkDataKey(key: KeyT): DataKey = StorageDataKey(key)

  private case class StorageDataKey(key: KeyT) extends DataKey {
    override def reload(height: Int): Unit = storage.reload(height, key)
  }
}
