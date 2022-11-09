package com.wavesplatform.collections

// TODO remove?
trait MapSyntax {
  @`inline` implicit final def mapSyntax[K <: AnyRef, V](self: Map[K, V]): MapOps[K, V] = new MapOps(self)
}

final class MapOps[K, V](private val self: Map[K, V]) extends AnyVal {
  def combineByKeys(other: Iterable[(K, V)])(combine: (V, V) => V): Map[K, V] =
    other.foldLeft(self) { case (r, (k, v)) =>
      r.updatedWith(k) {
        case Some(orig) => Some(combine(orig, v))
        case None       => Some(v)
      }
    }
}
