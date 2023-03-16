package com.wavesplatform.riderunner.storage

import cats.syntax.option.*
import com.github.benmanes.caffeine.cache.Caffeine
import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.meta.getSimpleName
import com.wavesplatform.riderunner.app.RideRunnerMetrics.rideStorageKeyNumberFor
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.Height
import com.wavesplatform.utils.ScorexLogging
import kamon.instrumentation.caffeine.KamonStatsCounter
import org.eclipse.collections.impl.map.mutable.ConcurrentHashMap

import scala.util.chaining.*

/** Exact, because stores not only a value, but an absence of it too
  */
trait ExactWithHeightStorage[KeyT <: AnyRef, ValueT, TagT] extends ScorexLogging {
  storage =>
  def settings: ExactWithHeightStorage.Settings

  private val name          = getSimpleName(this)
  private val numberCounter = rideStorageKeyNumberFor(name)

  // TODO use indexes!!!!

  // We can look up tags to determine if a key has been known, because tags are always in RAM
  // not exist - we don't known this key
  // exist, but empty - we known this key, but doesn't remember for what
  // exist and non-empty - we exactly know, why do we need this key
//  protected val tags = Caffeine
//    .newBuilder()
//    .recordStats(() => new KamonStatsCounter(s"$name.tags"))
//    .build[KeyT, Set[TagT]]()
  protected val tags = new ConcurrentHashMap[KeyT, Set[TagT]]()

  def load()(implicit ctx: ReadOnly): Unit = {
    persistentCache.getAllKeys().foreach(x => tags.put(x, Set.empty[TagT]))
    numberCounter.increment(tags.size())
  }

  protected val values = Caffeine
    .newBuilder()
    .softValues()
    .maximumSize(settings.maxEntries)
    .recordStats(() => new KamonStatsCounter(s"$name.values"))
    .build[KeyT, RemoteData[ValueT]]()

  private def tagsOf(key: KeyT): Option[Set[TagT]] = Option(tags.get(key))

  def getFromBlockchain(key: KeyT): Option[ValueT]

  def persistentCache: PersistentCache[KeyT, ValueT]

  // For REST API
  def getUntagged(atMaxHeight: Height, key: KeyT)(implicit ctx: ReadWrite): Option[ValueT] = getLatestInternal(atMaxHeight, key, None)

  // For running scripts
  def get(atMaxHeight: Height, key: KeyT, tag: TagT)(implicit ctx: ReadWrite): Option[ValueT] = getLatestInternal(atMaxHeight, key, Some(tag))

  // Only for REST API or running scripts
  private def getLatestInternal(atMaxHeight: Height, key: KeyT, tag: Option[TagT])(implicit ctx: ReadWrite): Option[ValueT] = {
    tag.foreach { tag =>
      // TODO if contains one value - then it wasn't before, optimize values insertion
      tags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)
    }

    values
      .get(
        key,
        { (key: KeyT) =>
          val cached = persistentCache.get(atMaxHeight, key)
          if (cached.loaded) cached
          else // TODO #36 This could be updated in a different thread
            RemoteData
              .loaded(getFromBlockchain(key))
              .tap { r =>
                persistentCache.set(atMaxHeight, key, r)
                numberCounter.increment()
              }
        }
      )
      .mayBeValue
  }

  def append(atHeight: Height, key: KeyT, update: ValueT)(implicit ctx: ReadWrite): AffectedTags[TagT] = append(atHeight, key, update.some)

  def append(atHeight: Height, key: KeyT, update: Option[ValueT])(implicit ctx: ReadWrite): AffectedTags[TagT] = tagsOf(key) match {
    case None => AffectedTags.empty
    case Some(tags) =>
      val orig = Option(values.getIfPresent(key))
        .getOrElse(RemoteData.Unknown) // The data was evicted from the memory cache
        .orElse(persistentCache.get(atHeight, key))

      if (orig.loaded) {
        val updated = RemoteData.loaded(update)
        log.debug(s"$key appended: $updated")

        // TODO #36 This could be updated in a different thread
        persistentCache.set(atHeight, key, updated) // Write here, because we want to preserve the height in any case

        if (updated == orig) AffectedTags.empty
        else {
          values.put(key, updated)
          AffectedTags(tags)
        }
      } else AffectedTags.empty // Not present in cache, also we haven't received this key
  }

  def undoAppend(toHeight: Height, key: KeyT)(implicit ctx: ReadWrite): AffectedTags[TagT] = rollback(toHeight, key, None)

  def rollback(toHeight: Height, key: KeyT, after: ValueT)(implicit ctx: ReadWrite): AffectedTags[TagT] = rollback(toHeight, key, after.some)

  // Micro blocks don't affect, because we know new values
  def rollback(toHeight: Height, key: KeyT, afterRollback: Option[ValueT])(implicit ctx: ReadWrite): AffectedTags[TagT] = tagsOf(key) match {
    case None => AffectedTags.empty
    case Some(tags) =>
      val beforeRollback = persistentCache.remove(toHeight + 1, key)
      afterRollback match {
        case None =>
          log.debug(if (beforeRollback.loaded) s"$key reloaded: ${beforeRollback.mayBeValue}" else s"$key: unknown")
          values.put(key, beforeRollback) // RemoteData.Unknown will be updated later during the run

        case Some(after) =>
          val restored = RemoteData.Cached(after)
          // This could be a rollback to a micro block
          if (beforeRollback != restored) persistentCache.set(toHeight, key, restored) // TODO #36 This could be updated in a different thread
          values.put(key, restored)
          log.trace(s"$key restored: $restored")
      }
      AffectedTags(tags)
  }
}

object ExactWithHeightStorage {
  case class Settings(maxEntries: Int)
}
