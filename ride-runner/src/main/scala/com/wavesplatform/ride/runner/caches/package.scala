package com.wavesplatform.ride.runner

package object caches {
  type AffectedTags[T] = Set[T]
  object AffectedTags {
    def empty[TagT]: AffectedTags[TagT] = Set.empty
  }
}
