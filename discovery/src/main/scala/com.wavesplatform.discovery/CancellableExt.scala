package com.wavesplatform.discovery

import akka.actor.Cancellable

object CancellableExt {
  implicit def Ext(self: Cancellable) = new {
    def combine(other: Cancellable): Cancellable = new Cancellable {
      override def cancel() = self.cancel() & other.cancel()

      override def isCancelled = self.isCancelled && other.isCancelled
    }
  }
}
