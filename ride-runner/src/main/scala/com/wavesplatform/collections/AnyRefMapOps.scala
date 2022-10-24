package com.wavesplatform.collections

import scala.collection.mutable

trait AnyRefMapSyntax {
  @`inline` implicit final def anyRefMapSyntaxRange[K <: AnyRef, V](self: mutable.AnyRefMap[K, V]): AnyRefMapOps[K, V] = new AnyRefMapOps(self)
}

final class AnyRefMapOps[K <: AnyRef, V](val self: mutable.AnyRefMap[K, V]) extends AnyVal {

  /** @return
    *   Updated?
    */
  def replaceIfExists(key: K)(remap: V => Option[V]): Boolean = {
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
}
