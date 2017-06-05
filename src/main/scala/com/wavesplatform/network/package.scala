package com.wavesplatform

import java.net.{InetSocketAddress, SocketAddress, URI}
import java.util.concurrent.Callable

import io.netty.channel.{Channel, ChannelHandlerContext}
import io.netty.util.AttributeKey
import io.netty.util.concurrent.{EventExecutorGroup, ScheduledFuture}

import scala.concurrent.duration._

package object network {
  def inetSocketAddress(addr: String, defaultPort: Int): InetSocketAddress = {
    val uri = new URI(s"my://$addr")
    if (uri.getPort < 0) new InetSocketAddress(addr, defaultPort)
    else new InetSocketAddress(uri.getHost, uri.getPort)
  }

  def sameAddresses(a1: SocketAddress, a2: SocketAddress): Boolean = a1 == a2 ||
    ((a1, a2) match {
      case (isa1: InetSocketAddress, isa2: InetSocketAddress) => isa1.getAddress == isa2.getAddress
      case _ => false
    })

  implicit class EventExecutorGroupExt(val e: EventExecutorGroup) extends AnyVal {
    def scheduleWithFixedDelay(initialDelay: FiniteDuration, delay: FiniteDuration)(f: => Unit): ScheduledFuture[_] =
      e.scheduleWithFixedDelay((() => f): Runnable, initialDelay.toNanos, delay.toNanos, NANOSECONDS)

    def schedule[A](delay: FiniteDuration)(f: => A): ScheduledFuture[A] =
      e.schedule((() => f): Callable[A], delay.length, delay.unit)
  }

  val HandshakeKey = AttributeKey.newInstance[Handshake]("handshake")

  def id(ctx: ChannelHandlerContext): String = id(ctx.channel())
  def id(chan: Channel): String = s"[${chan.id().asShortText()}: ${chan.attr(AttributeKeys.NodeName)}@${chan.attr(AttributeKeys.RemoteAddress)}]"
}
