package com.wavesplatform.collections

import scala.collection.mutable

trait AbstractMapSyntax {
  @`inline` implicit final def abstractMapSyntax[K <: AnyRef, V](self: mutable.AbstractMap[K, V]): AbstractMapOps[K, V] = new AbstractMapOps(self)
}

final class AbstractMapOps[K <: AnyRef, V](val self: mutable.AbstractMap[K, V]) extends AnyVal {

  /** @return
    *   Updated?
    */
  def replaceIfExists(key: K)(remap: V => Option[V]): Boolean =
    self.get(key) match {
      case Some(orig) =>
        remap(orig) match {
          case Some(updated) /*if updated != orig*/ =>
            self.update(key, updated)
            true

          case None =>
            self.remove(key)
            true

          case _ => false
        }

      case None => false
    }
}
