package com.wavesplatform

import java.net.{InetSocketAddress, URI}

import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}

import scala.concurrent.duration._

package object network {
  def inetSocketAddress(addr: String, defaultPort: Int): InetSocketAddress = {
    val uri = new URI(s"my://$addr")
    if (uri.getPort < 0) new InetSocketAddress(addr, defaultPort)
    else new InetSocketAddress(uri.getHost, uri.getPort)
  }

  implicit class EventExecutorGroupExt(val e: EventExecutorGroup) extends AnyVal {
    def scheduleWithFixedDelay(initialDelay: FiniteDuration, delay: FiniteDuration)(f: => Unit): ScheduledFuture[_] =
      e.scheduleWithFixedDelay((() => f): Runnable, initialDelay.toNanos, delay.toNanos, NANOSECONDS)
  }
}
