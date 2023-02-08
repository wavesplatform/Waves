package com.wavesplatform.storage

import cats.syntax.option.*
import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.utils.ScorexLogging
import kamon.instrumentation.caffeine.KamonStatsCounter

import scala.util.chaining.*

/** Exact, because stores not only a value, but an absence of it too
  */
trait ExactWithHeightStorage[KeyT <: AnyRef, ValueT, TagT] extends ScorexLogging {
  storage =>
  def settings: ExactWithHeightStorage.Settings

  // We can look up tags to determine if a key has been known, because tags are always in RAM
  protected val tags = Caffeine
    .newBuilder()
    .recordStats(() => new KamonStatsCounter(s"$name.tags"))
    .build[KeyT, Set[TagT]]()

  protected val values = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(settings.maxEntries)
    .recordStats(() => new KamonStatsCounter(s"$name.values"))
    .build[KeyT, RemoteData[ValueT]]()

  private def tagsOf(key: KeyT): Set[TagT] = Option(tags.getIfPresent(key)).getOrElse(Set.empty)

  lazy val name = getSimpleName(this)

  def getFromBlockchain(key: KeyT): Option[ValueT]

  def persistentCache: PersistentCache[KeyT, ValueT]

  // For REST API
  def getUntagged(height: Int, key: KeyT): Option[ValueT] = getInternal(height, key, None)

  // For running scripts
  def get(height: Int, key: KeyT, tag: TagT): Option[ValueT] = getInternal(height, key, Some(tag))

  // Only for REST API or running scripts
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
              .tap(r => persistentCache.set(height, key, r)) // TODO #36 This could be updated in a different thread
        }
      )
      .mayBeValue
  }

  def append(height: Int, key: KeyT, update: ValueT): AffectedTags[TagT] = append(height, key, update.some)

  def append(height: Int, key: KeyT, update: Option[ValueT]): AffectedTags[TagT] = {
    lazy val tags = tagsOf(key)
    val orig =
      if (tags.isEmpty) RemoteData.Unknown // We haven't known this key
      else
        Option(values.getIfPresent(key))
          .getOrElse(RemoteData.Unknown) // The data was evicted from the memory cache
          .orElse(persistentCache.get(height, key))

    if (orig.loaded) {
      val updated = RemoteData.loaded(update)
      log.debug(s"$key appended: $updated")

      // TODO #36 This could be updated in a different thread
      persistentCache.set(height, key, updated) // Write here (not in "else"), because we want to preserve the height

      if (updated == orig) AffectedTags.empty
      else {
        values.put(key, updated)
        AffectedTags(tags)
      }
    } else AffectedTags.empty // Not present in cache, also we haven't received this key
  }

  def undoAppend(rollbackHeight: Int, key: KeyT): AffectedTags[TagT] = rollback(rollbackHeight, key, None)

  def rollback(rollbackHeight: Int, key: KeyT, after: ValueT): AffectedTags[TagT] = rollback(rollbackHeight, key, after.some)

  // Micro blocks don't affect, because we know new values
  def rollback(rollbackHeight: Int, key: KeyT, afterRollback: Option[ValueT]): AffectedTags[TagT] = {
    val tags = tagsOf(key)
    if (tags.isEmpty) AffectedTags.empty
    else {
      val beforeRollback = persistentCache.remove(rollbackHeight + 1, key)
      afterRollback match {
        case None =>
          log.debug(if (beforeRollback.loaded) s"$key reloaded: ${beforeRollback.mayBeValue}" else s"$key: unknown")
          values.put(key, beforeRollback) // RemoteData.Unknown will be updated later during the run
          AffectedTags(tags)

        case Some(after) =>
          val restored = RemoteData.Cached(after)
          // This could be a rollback to a micro block
          if (beforeRollback != restored) persistentCache.set(rollbackHeight, key, restored) // TODO #36 This could be updated in a different thread
          values.put(key, restored)
          log.trace(s"$key restored: $restored")
          AffectedTags(tags)
      }
    }
  }
}

object ExactWithHeightStorage {
  case class Settings(maxEntries: Int)
}
