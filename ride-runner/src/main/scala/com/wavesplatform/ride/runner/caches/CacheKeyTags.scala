package com.wavesplatform.ride.runner.caches

import com.wavesplatform.ride.runner.caches.mem.MemCacheKey

import java.util.concurrent.ConcurrentHashMap

class CacheKeyTags[TagT] {
  private type Tags = Set[TagT]
  private val emptyTags: Tags = Set.empty[TagT]

  // We can look up tags to determine if a key has been known, because tags are always in RAM
  //   not exist - we don't known this key
  //   exist, but empty - we known this key, but doesn't remember why
  //   exist and non-empty - we know, why do we need this key (but there are probably more tags)
  private val allTags = new ConcurrentHashMap[MemCacheKey, Tags]()

  def get(key: MemCacheKey): Option[Tags] = Option(allTags.get(key))

  def addDependent(key: MemCacheKey, tag: TagT): Unit = allTags.compute(key, (_, origTags) => Option(origTags).getOrElse(Set.empty) + tag)

  def removeTags(xs: collection.Set[TagT]): Unit = {
    allTags.replaceAll { (_: MemCacheKey, orig: Tags) =>
      if (orig.size >= xs.size) orig -- xs
      else orig.diff(xs)
    }

    // Use removeIf to remove empty keys in JRE 17
    val emptyKeys = allTags.reduceEntries[Seq[MemCacheKey]](
      4, // parallelismThreshold
      entry => if (entry.getValue.isEmpty) Seq(entry.getKey) else Seq.empty,
      _ ++ _
    )

    emptyKeys.foreach(k => allTags.remove(k, emptyTags)) // ?????
  }
}
